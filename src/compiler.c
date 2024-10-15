#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif
typedef struct {
  Token current;
  Token previous;
  bool hadError;
  bool panicMode;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_UNARY,      // ! -
  PREC_CALL,       // . ()
  PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

typedef struct {
  Token name;
  int depth;
  bool isCaptured;
} Local;

typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;

typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT,
  TYPE_METHOD,
  TYPE_INITIALIZER
} FunctionType;

typedef struct Compiler {
  struct Compiler *
      enclosing; // can't reference the `Compiler` typedef because as of
                 // `enclosing`'s definition, `Compiler` isn't fully defined yet
  ObjFunction *function;
  FunctionType type;
  Local locals[UINT8_COUNT];
  int localCount;
  Upvalue upvalues[UINT8_COUNT];
  int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
  struct ClassCompiler *enclosing;
} ClassCompiler;

Compiler *current = NULL;
ClassCompiler *currentClass = NULL;

Parser parser;
Chunk *compilingChunk;

static void initCompiler(Compiler *compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function =
      NULL; // memory zeroing to avoid weird edge cases re: garbage collection
  compiler->type = type;
  compiler->localCount = 0;
  compiler->scopeDepth = 0;

  // Creating an ObjFunction in the compiler might seem a little strange. A
  // function object is the runtime representation of a function, but here we
  // are creating it at compile time. The way to think of it is that a function
  // is similar to a string or number literal. It forms a bridge between the
  // compile time and runtime worlds. When we get to function declarations,
  // those really are literals -- they are a notation that produces values of a
  // built-in type. So the compiler creates function objects during compilation.
  // Then, at runtime, they are simply invoked.
  compiler->function = newFunction();
  current = compiler;
  if (type != TYPE_SCRIPT) {
    current->function->name = copyString(
        parser.previous.start,
        parser.previous
            .length); // Remember, the lexeme from parser points directly into
                      // the original source code string. That string may get
                      // freed once the code is finished compiling. The function
                      // object we create in the compiler outlives the compiler
                      // and persists until runtime. So it needs its own
                      // heap-allocated name string that it can keep around.
  }

  // Compiler's locals array keeps track of which stack slots are associated
  // with which local variables or temporaries. From now on, the compiler
  // implicitly claims stack slot zero for the VM's own internal use.
  Local *local = &current->locals[current->localCount++];
  local->depth = 0;
  local->name.start = "";
  local->name.length = 0;
  local->isCaptured = false;

  if (type != TYPE_FUNCTION) {
    local->name.start = "this";
    local->name.length = 4;
  } else {
    local->name.start = "";
    local->name.length = 0;
  }
}

static Chunk *currentChunk() { return &current->function->chunk; }

static void errorAt(Token *token, const char *message) {
  if (parser.panicMode) {
    return;
  }
  parser.panicMode = true;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char *message) { errorAt(&parser.previous, message); }

static void errorAtCurrent(const char *message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;
  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR) {
      break;
    }
    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char *message) {
  if (parser.current.type == type) {
    advance();
    return;
  }
  errorAtCurrent(message);
}

static bool check(TokenType type) { return parser.current.type == type; }

static bool match(TokenType type) {
  if (!check(type)) {
    return false;
  }
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}
static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}
static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);
  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX)
    error("Loop body too large.");
  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}
static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  emitByte(0xff);
  emitByte(0xff);
  return currentChunk()->count - 2;
}
static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = currentChunk()->count - offset - 2;
  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }
  currentChunk()->code[offset] =
      (jump >> 8) & 0xff; // this writes the higher 8 bits
  currentChunk()->code[offset + 1] = jump & 0xff; // this writes the lower 8
}

static void emitReturn() {
  if (current->type == TYPE_INITIALIZER) {
    // In an initializer, instead of pushing `nil` onto the stack before
    // returning, we load slot zero, which contains the instance mapped to
    // `this`. This function is also called when compiling a return statement
    // without a value, so this also correctly handles cases where the user does
    // an early return inside the initializer.
    emitBytes(OP_GET_LOCAL, 0);
  } else {
    emitByte(OP_NIL);
  }
  emitByte(OP_RETURN);
}
static ObjFunction *endCompiler() {
  emitReturn();
  ObjFunction *function = current->function;
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), function->name != NULL
                                         ? function->name->chars
                                         : "<script>");
  }
#endif
  current = current->enclosing;
  return function;
}

static void beginScope() { current->scopeDepth++; }
static void endScope() {
  current->scopeDepth--;
  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
    if (current->locals[current->localCount - 1].isCaptured) {
      emitByte(OP_CLOSE_UPVALUE);
    } else {
      emitByte(OP_POP);
    }
    current->localCount--;
  }
}

static void expression();
static void statement();
static void declaration();
static uint8_t makeConstant(Value value);
static ParseRule *getRule(TokenType type);

static void parsePrecedence(Precedence precedence) {
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }
  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  // ParseFns (we only care about variables) won't consume the `=` if invalid
  // assign target. So (canAssign && match(TOKEN_EQUAL)) indicates that the `=`
  // was not consumed.
  // TODO: consider in your own implementations if you can do better than this.
  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }
}
static uint8_t identifierConstant(Token *name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}
static bool identifiersEqual(Token *a, Token *b) {
  if (a->length != b->length)
    return false;
  return memcmp(a->start, b->start, a->length) == 0;
}
static int resolveLocal(Compiler *compiler, Token *name) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local *local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Can't read local variable in its own initializer.");
      }
      return i;
    }
  }
  return -1;
}
static int addUpvalue(Compiler *compiler, uint8_t index, bool isLocal) {
  int upvalueCount = compiler->function->upvalueCount;

  // A closure may reference the same variable in a surrounding function
  // multiple times. In that case, we don't want to waste time and memory
  // creating a separate upvalue for each identifier expression. To fix that,
  // before we add a new upvalue, we first check to see if the function already
  // has an upvalue that closes over that variable.
  for (int i = 0; i < upvalueCount; i++) {
    Upvalue *upvalue = &compiler->upvalues[i];
    // if we find an upvalue in the array whose slot index matches the one we're
    // adding, we just return that upvalue index and reuse it.
    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    error("Too many closure variables in function.");
    return 0;
  }

  // Otherwise, we fall through and add the new upvalue.
  compiler->upvalues[upvalueCount].isLocal = isLocal;
  compiler->upvalues[upvalueCount].index = index;
  return compiler->function
      ->upvalueCount++; // like constants and function arity, the upvalue count
                        // is another one of those little pieces of data that
                        // form the bridge between the compiler and runtime.
}
static int resolveUpValue(Compiler *compiler, Token *name) {
  if (compiler->enclosing == NULL)
    return -1;
  int local = resolveLocal(compiler->enclosing, name);
  if (local != -1) {
    compiler->enclosing->locals[local].isCaptured = true;
    return addUpvalue(compiler, (uint8_t)local, true);
  }

  int upvalue = resolveUpValue(compiler->enclosing, name);
  if (upvalue != -1) {
    return addUpvalue(compiler, (uint8_t)upvalue, false);
  }
  return -1;
}
static void addLocal(Token name) {
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }
  Local *local = &current->locals[current->localCount++];
  local->name =
      name; // local directly stores a copy of the Token struct for the
            // identifier, which in turn stores a pointer to the first character
            // (and length) in the source string which is of lifetime `static`.
            // Thus addressing string-lifetime concerns.
  local->depth = current->scopeDepth;
  local->depth = -1;
  local->isCaptured = false;
}

// This is where the compiler records the existence of locals.
// Globals are late bound, so the compiler doesn't keep track of which global
// declarations it has seen.
static void declareVariable() {
  if (current->scopeDepth == 0) {
    return;
  }
  Token *name = &parser.previous;
  // Local vars are appended to the array when they're declared, which means the
  // current scope is always at the end of the array. When we declare a new
  // variable, we start at the end and work backward, checking for the error
  // case where variable identifiers match, until we back out into an outer
  // scope (which we identify by checking the depth of the local we are
  // visiting).
  for (int i = current->localCount - 1; i >= 0; i--) {
    Local *local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }
    if (identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }
  addLocal(*name);
}
static uint8_t parseVariable(const char *errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
  declareVariable();
  if (current->scopeDepth > 0) {
    // We exit the function early if we are in a local scope.
    // At runtime, locals aren't looked up by name.
    // There's no need to stuff the variable's name into the constant table,
    // so if the declaration is inside a local scope, we return a dummy table
    // index instead.
    return 0;
  }
  return identifierConstant(&parser.previous);
}
static void markInitialized() {
  if (current->scopeDepth == 0)
    return;
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}
static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    // No code to create a local variable at runtime.
    // VM has already executed the code for the var's initializer, and
    // that value is sitting right on top of the stack as the only remaining
    // temporary.
    markInitialized();
    return;
  }
  emitBytes(OP_DEFINE_GLOBAL, global);
}
static uint8_t argumentList() {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();
      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}
static void and_(bool canAssign) {
  int endJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  parsePrecedence(PREC_AND);
  patchJump(endJump);
}
static void expression() { parsePrecedence(PREC_ASSIGNMENT); }
static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}
static void funcBody(FunctionType type) {
  Compiler compiler;
  initCompiler(&compiler,
               type); // creating a scope-local compiler to leverage the fact
                      // that compilers output functions... interesting! Note,
                      // however, that `initCompiler` reassigns the `current`
                      // compiler with the inner one, so you need to have a way
                      // to back out to the enclosing compiler (which is why the
                      // `Compiler` struct is implemented as a linked list)
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;
      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }
      uint8_t constant = parseVariable("Expect parameter name.");
      defineVariable(constant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();

  ObjFunction *function = endCompiler();
  emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

  // The `OP_CLOSURE` instruction is unique in that it has a variably-sized
  // encoding. For each upvalue the closure captures, there are two single-byte
  // operands. Each pair of operands specifies what that upvalue captures. If
  // the first byte is one, it captures a local variable in the enclosing
  // function. If zero, it captures one of the function's upvalues. The next
  // byte is the local slot or the upvalue index to capture.
  for (int i = 0; i < function->upvalueCount; i++) {
    emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
    emitByte(compiler.upvalues[i].index);
  }
}
static void funDeclaration() {
  uint8_t global = parseVariable(
      "Expect function name."); // func declarations just get treated as
                                // variables. If top-level decl, treated as
                                // global variable. functions declared within
                                // other functions get treated as local
                                // variables.
  markInitialized(); // We can safely mark function declarations as initialized;
                     // this step served to prevent variables from being
                     // accessible from within their own initializer, but we
                     // actually want this behavior for functions to support
                     // local recursion. Anyway, a function can't be called
                     // until its definition is completed.
  funcBody(TYPE_FUNCTION);
  defineVariable(global);
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else if ((arg = resolveUpValue(current, &name)) != -1) {
    getOp = OP_GET_UPVALUE;
    setOp = OP_SET_UPVALUE;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }
  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(setOp, (uint8_t)arg);
  } else {
    emitBytes(getOp, (uint8_t)arg);
  }
}
static void method() {
  consume(TOKEN_IDENTIFIER, "Expect method name.");
  uint8_t constant = identifierConstant(&parser.previous);
  FunctionType type = TYPE_METHOD;
  if (parser.previous.length == 4 &&
      memcmp(parser.previous.start, "init", 4) == 0) {
    type = TYPE_INITIALIZER;
  }
  funcBody(type);
  emitBytes(OP_METHOD, constant);
}
static void classDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expect class name.");
  Token className = parser.previous;
  uint8_t nameConstant = identifierConstant(&parser.previous);
  declareVariable();

  emitBytes(OP_CLASS, nameConstant);
  defineVariable(nameConstant);

  ClassCompiler classCompiler;
  classCompiler.enclosing = currentClass;
  currentClass = &classCompiler;

  // Right before compiling the class body, we generate code to load a variable
  // with the class name onto the stack. Then we compile the methods. This means
  // that when we execute each `OP_METHOD` instricution, the stack has the
  // method's closure on top with the class right under it. Once we've reached
  // the end of the methods, we no longer need the class and tell the VM to pop
  // it off the stack.
  namedVariable(className, false);
  consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    method();
  }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
  emitByte(OP_POP);
  currentClass = currentClass->enclosing;
}
static void varDeclaration() {
  uint8_t global = parseVariable("Expect variable name.");
  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
  defineVariable(global);
}
static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");

  // Semantically, an expression statement evaluates the expression and
  // discards the result. The compiler directly encodes that behavior. It
  // compiles the expression, and emits a pop instruction. (because the
  // expression call pushes onto the stack directly);
  emitByte(OP_POP);
}
static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(TOKEN_SEMICOLON)) {
    // no initializer
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }
  int loopStart = currentChunk()->count;
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
  }
  // The increment clause is pretty convoluted, for it appears textually before
  // the body, but executes after it.
  // We cannot compile the increment clause later, because we are building a
  // single-pass compiler. Instead, we'll jump over the increment, run the body,
  // jump back up to the increment, run it, then go to the next iteration.
  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  statement();
  emitLoop(loopStart);
  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP); // clean up stack value for condition expression eval
  }
  endScope();
}
static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  // We have a problem here. When we're writing the jump-if-false instruction,
  // we don't have the operand yet (jump by how much?). We haven't compiled the
  // then branch yet, so we don't know how much bytecode it contains. To fix
  // that, we do `backpatching`. We emit the jump instruction first with a
  // placeholder offset operand.
  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement(); // then block
  int elseJump = emitJump(OP_JUMP);
  // We keep track of where that half-finished instruction is. Next, we compile
  // the then body. After that, we can see how many operations the `then` block
  // needed to backfill the jump instruction's operand.
  patchJump(thenJump);
  emitByte(OP_POP);

  if (match(TOKEN_ELSE))
    statement(); // else block
  patchJump(elseJump);
}
static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "expect ';' after value.");
  emitByte(OP_PRINT);
}

// Observe here that returning works quite differently from jlox, because there
// is nestedness only at the compilation step in clox. The compiled bytecode is
// all flat, because it explicitly manipulates instruction offsets whereas jlox
// made use of the instruction offsets built into the jvm call stack.
static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    error("Can't return from top-level code.");
  }
  if (match(TOKEN_SEMICOLON)) {
    emitReturn();
  } else {
    if (current->type == TYPE_INITIALIZER) {
      error("Can't return a value from an initializer.");
    }
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
    emitByte(OP_RETURN);
  }
}
static void whileStatement() {
  int loopStart = currentChunk()->count;
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
  int endJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  emitLoop(loopStart);
  patchJump(endJump);
  emitByte(OP_POP);
}
static void synchronize() {
  parser.panicMode = false;
  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON) {
      return;
    }
    switch (parser.current.type) {
    case TOKEN_CLASS:
    case TOKEN_FUN:
    case TOKEN_VAR:
    case TOKEN_FOR:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_PRINT:
    case TOKEN_RETURN:
      return;

    default:; // Do nothing.
    }
    advance();
  }
}
static void declaration() {
  if (match(TOKEN_CLASS)) {
    classDeclaration();
  } else if (match(TOKEN_FUN)) {
    funDeclaration();
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }

  if (parser.panicMode) {
    synchronize();
  }
}
static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_RETURN)) {
    returnStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

static void unary(bool canAssign) {
  TokenType operatorType = parser.previous.type;

  // compile the operand.
  parsePrecedence(PREC_UNARY);

  // Emit the operator instruction.
  switch (operatorType) {
  case TOKEN_MINUS:
    emitByte(OP_NEGATE); // remember stack semantics; negate instruction
                         // should be emitted last
    break;
  case TOKEN_BANG:
    emitByte(OP_NOT);
    break;
  default:
    return;
  }
}

static void binary(bool canAssign) {
  TokenType operatorType = parser.previous.type;
  ParseRule *rule = getRule(operatorType);

  // We use one higher level of precedence for the right operand because the
  // binary operators are left-associative.
  // Given a series of the same operator, like:
  // 1 + 2 + 3 + 4
  // We want to parse it like:
  // ((1 + 2) + 3) + 4
  // Thus, when parsing the right-hand operand to the first +, we want to
  // consume the 2, but not the rest, so we use one level above +’s
  // precedence. But if our operator was right-associative, this would be
  // wrong. Given: a = b = c = d Since assignment is right-associative, we
  // want to parse it as: a = (b = (c = d)) To enable that, we would call
  // parsePrecedence() with the *same* precedence as the current operator.
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
  case TOKEN_BANG_EQUAL:
    emitBytes(OP_EQUAL, OP_NOT);
    break;
  case TOKEN_EQUAL_EQUAL:
    emitByte(OP_EQUAL);
    break;
  case TOKEN_GREATER:
    emitByte(OP_GREATER);
    break;
  case TOKEN_GREATER_EQUAL:
    emitBytes(OP_LESS, OP_NOT);
    break;
  case TOKEN_LESS:
    emitByte(OP_LESS);
    break;
  case TOKEN_LESS_EQUAL:
    emitBytes(OP_GREATER, OP_NOT);
    break;
  case TOKEN_PLUS:
    emitByte(OP_ADD);
    break;
  case TOKEN_MINUS:
    emitByte(OP_SUBTRACT);
    break;
  case TOKEN_STAR:
    emitByte(OP_MULTIPLY);
    break;
  case TOKEN_SLASH:
    emitByte(OP_DIVIDE);
    break;
  default:
    return; // Unreachable.
  }
}

static void call(bool canAssign) {
  uint8_t argCount = argumentList();
  emitBytes(OP_CALL, argCount);
}
static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = identifierConstant(&parser.previous);

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(OP_SET_PROPERTY, name);
  } else {
    emitBytes(OP_GET_PROPERTY, name);
  }
}

static void literal(bool canAssign) {
  switch (parser.previous.type) {
  case TOKEN_FALSE:
    emitByte(OP_FALSE);
    break;
  case TOKEN_NIL:
    emitByte(OP_NIL);
    break;
  case TOKEN_TRUE:
    emitByte(OP_TRUE);
    break;
  default:
    return; // Unreachable.
  }
}

/// As far as the back end is concerned, there's literally nothing to a
/// grouping expression. It's sole function is syntactic -- it lets you insert
/// a lower-precedence expression where a higher precedence is expected.
/// Thus, it has no runtime semantics on its own and therefore doesn't emit
/// any bytecode. jthe inner call to expression() takes care of generating
/// byte code for the expression inside the parentheses.
static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }
  return (uint8_t)constant;
}
static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void number(bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}
static void or_(bool canAssign) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);
  patchJump(elseJump);
  emitByte(OP_POP);
  parsePrecedence(PREC_OR);
  patchJump(endJump);
}
static void string(bool canAssign) {
  // starts after the opening quote of a string, and runs for length, less the
  // quote characters.
  emitConstant(OBJ_VAL(
      copyString(parser.previous.start + 1, parser.previous.length - 2)));
}
static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}
static void this_(bool canAssign) {
  if (currentClass == NULL) {
    error("Can't use 'this' outside of a class.");
    return;
  }
  variable(false);
}
static void grouping(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] =
        {grouping, call,
         PREC_CALL}, // We don't usually thinkg of it this way, but a function
                     // call expression is kind of an infix `(` operator. You
                     // have a high-precedence expression on the left for the
                     // thing being called - usually just a single identifier.
                     // Then the `(` in the middle, followed by the argument
                     // expressions separated by commas, then a final `)` to
                     // wrap it up at the end.
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, dot, PREC_CALL},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_NONE},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {this_, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};
static ParseRule *getRule(TokenType type) { return &rules[type]; }

ObjFunction *compile(const char *source) {
  initScanner(source);
  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);
  parser.hadError = false;
  parser.panicMode = false;
  advance();
  while (!match(TOKEN_EOF)) {
    declaration();
  }
  ObjFunction *function = endCompiler();
  return parser.hadError ? NULL : function;
}
void markCompilerRoots() {
  Compiler *compiler = current;
  while (compiler != NULL) {
    // The only object the compiler uses is the ObjFunction which it is
    // compiling into. Since the functino declarations can nest, the compiler
    // has a linked list of those and we walk the whole list.
    markObject((Obj *)compiler->function);
    compiler = compiler->enclosing;
  }
}
