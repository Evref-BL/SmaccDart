%glr;
/*grammar Dart;*/

/* TODO: @parser::header{
import java.util.Stack;
}*/

/* TODO: @lexer::header{
import java.util.Stack;
}*/

/* TODO: @parser::members {
  static String filePath = null;
  static boolean errorHasOccurred = false;

  
  
  static void prepareForErrors() {
    errorHasOccurred = true;
    System.err.println("Syntax error in " + filePath + ":");
  }

  
  public boolean parseLibrary(String filePath) throws RecognitionException {
    this.filePath = filePath;
    errorHasOccurred = false;
    libraryDefinition();
    return !errorHasOccurred;
  }

  
  
  private Stack<Boolean> asyncEtcAreKeywords = new Stack<Boolean>();
  { asyncEtcAreKeywords.push(false); }

  
  
  void startAsyncFunction() { asyncEtcAreKeywords.push(true); }

  
  
  void startNonAsyncFunction() { asyncEtcAreKeywords.push(false); }

  
  void endFunction() { asyncEtcAreKeywords.pop(); }

  
  boolean asyncEtcPredicate(int tokenId) {
    if (tokenId == AWAIT || tokenId == YIELD) {
      return !asyncEtcAreKeywords.peek();
    }
    return false;
  }
}*/

/* TODO: @lexer::members{
  public static final int BRACE_NORMAL = 1;
  public static final int BRACE_SINGLE = 2;
  public static final int BRACE_DOUBLE = 3;
  public static final int BRACE_THREE_SINGLE = 4;
  public static final int BRACE_THREE_DOUBLE = 5;
  
  
  private Stack<Integer> braceLevels = new Stack<Integer>();

  
  boolean currentBraceLevel(int braceLevel) {
    if (braceLevels.empty()) return false;
    return braceLevels.peek() == braceLevel;
  }

  void enterBrace() {
    braceLevels.push(BRACE_NORMAL);
  }
  void enterBraceSingleQuote() {
    braceLevels.push(BRACE_SINGLE);
  }
  void enterBraceDoubleQuote() {
    braceLevels.push(BRACE_DOUBLE);
  }
  void enterBraceThreeSingleQuotes() {
    braceLevels.push(BRACE_THREE_SINGLE);
  }
  void enterBraceThreeDoubleQuotes() {
    braceLevels.push(BRACE_THREE_DOUBLE);
  }

  
  void exitBrace() { 
      
      if (!braceLevels.empty()) braceLevels.pop();
  }
}*/



libraryDefinition
    :    <FEFF>? <SCRIPT_TAG>?
         libraryName?
         importOrExport*
         partDirective*
         (metadata topLevelDefinition)*
         
    ;

topLevelDefinition
    :    classDeclaration
    |    mixinDeclaration
    |    extensionDeclaration
    |    enumType
    |    typeAlias
    |    <EXTERNAL> functionSignature ";"
    |    <EXTERNAL> getterSignature ";"
    |    <EXTERNAL> setterSignature ";"
    |    <EXTERNAL> finalVarOrType identifierList ";"
    |    getterSignature functionBody
    |    setterSignature functionBody
    |    functionSignature functionBody
    |    (<FINAL> | <CONST>) type? staticFinalDeclarationList ";"
    |    <LATE> <FINAL> type? initializedIdentifierList ";"
    |    <LATE>? varOrType identifier ("=" expression)?
         ("," initializedIdentifier)* ";"
    ;

declaredIdentifier
    :    <COVARIANT>? finalConstVarOrType identifier
    ;

finalConstVarOrType
    :    <LATE>? <FINAL> type?
    |    <CONST> type?
    |    <LATE>? varOrType
    ;

finalVarOrType
    :    <FINAL> type?
    |    varOrType
    ;

varOrType
    :    <VAR>
    |    type
    ;

initializedIdentifier
    :    identifier ("=" expression)?
    ;

initializedIdentifierList
    :    initializedIdentifier ("," initializedIdentifier)*
    ;

functionSignature
    :    type? identifierNotFUNCTION formalParameterPart
    ;

functionBodyPrefix
    :    <ASYNC>? "=>"
    |    (<ASYNC> | <ASYNC> "*" | <SYNC> "*")? <LBRACE>
    ;

functionBody
    :    "=>" /* TODO: { startNonAsyncFunction(); }*/ expression /* TODO: { endFunction(); }*/ ";"
    |    /* TODO: { startNonAsyncFunction(); }*/ block /* TODO: { endFunction(); }*/
    |    <ASYNC> "=>"
         /* TODO: { startAsyncFunction(); }*/ expression /* TODO: { endFunction(); }*/ ";"
    |    (<ASYNC> | <ASYNC> "*" | <SYNC> "*")
         /* TODO: { startAsyncFunction(); }*/ block /* TODO: { endFunction(); }*/
    ;

block
    :    <LBRACE> statements <RBRACE>
    ;

formalParameterPart
    :    typeParameters? formalParameterList
    ;

formalParameterList
    :    "(" ")"
    |    "(" normalFormalParameters ","? ")"
    |    "(" normalFormalParameters "," optionalOrNamedFormalParameters ")"
    |    "(" optionalOrNamedFormalParameters ")"
    ;

normalFormalParameters
    :    normalFormalParameter ("," normalFormalParameter)*
    ;

optionalOrNamedFormalParameters
    :    optionalPositionalFormalParameters
    |    namedFormalParameters
    ;

optionalPositionalFormalParameters
    :    "[" defaultFormalParameter ("," defaultFormalParameter)* ","? "]"
    ;

namedFormalParameters
    :    <LBRACE> defaultNamedParameter ("," defaultNamedParameter)* ","? <RBRACE>
    ;

normalFormalParameter
    :    metadata normalFormalParameterNoMetadata
    ;

normalFormalParameterNoMetadata
    :    functionFormalParameter
    |    fieldFormalParameter
    |    simpleFormalParameter
    ;


functionFormalParameter
    :    <COVARIANT>? type? identifierNotFUNCTION formalParameterPart "?"?
    ;

simpleFormalParameter
    :    declaredIdentifier
    |    <COVARIANT>? identifier
    ;


fieldFormalParameter
    :    finalConstVarOrType? <THIS> "." identifier (formalParameterPart "?"?)?
    ;

defaultFormalParameter
    :    normalFormalParameter ("=" expression)?
    ;

defaultNamedParameter
    :    <REQUIRED>? normalFormalParameter ((":" | "=") expression)?
    ;

typeWithParameters
    :    typeIdentifier typeParameters?
    ;

classDeclaration
    :    <ABSTRACT>? <CLASS> typeWithParameters superclass? mixins? interfaces?
         <LBRACE> (metadata classMemberDefinition)* <RBRACE>
    |    <ABSTRACT>? <CLASS> mixinApplicationClass
    ;

superclass
    :    <EXTENDS> typeNotVoidNotFunction
    ;

mixins
    :    <WITH> typeNotVoidNotFunctionList
    ;

interfaces
    :    <IMPLEMENTS> typeNotVoidNotFunctionList
    ;

classMemberDefinition
    :    methodSignature functionBody
    |    declaration ";"
    ;

mixinApplicationClass
    :    typeWithParameters "=" mixinApplication ";"
    ;

mixinDeclaration
    :    <MIXIN> typeIdentifier typeParameters?
         (<ON> typeNotVoidNotFunctionList)? interfaces?
         <LBRACE> (metadata mixinMemberDefinition)* <RBRACE>
    ;


mixinMemberDefinition
    :    classMemberDefinition
    ;

extensionDeclaration
    :    <EXTENSION> identifier? typeParameters? <ON> type
         <LBRACE> (metadata extensionMemberDefinition)* <RBRACE>
    ;


extensionMemberDefinition
    :    classMemberDefinition
    ;

methodSignature
    :    constructorSignature initializers
    |    factoryConstructorSignature
    |    <STATIC>? functionSignature
    |    <STATIC>? getterSignature
    |    <STATIC>? setterSignature
    |    operatorSignature
    |    constructorSignature
    ;

declaration
    :    <EXTERNAL> factoryConstructorSignature
    |    <EXTERNAL> constantConstructorSignature
    |    <EXTERNAL> constructorSignature
    |    (<EXTERNAL> <STATIC>?)? getterSignature
    |    (<EXTERNAL> <STATIC>?)? setterSignature
    |    (<EXTERNAL> <STATIC>?)? functionSignature
    |    <EXTERNAL> (<STATIC>? finalVarOrType | <COVARIANT> varOrType) identifierList
    |    <ABSTRACT> (finalVarOrType | <COVARIANT> varOrType) identifierList
    |    <EXTERNAL>? operatorSignature
    |    <STATIC> (<FINAL> | <CONST>) type? staticFinalDeclarationList
    |    <STATIC> <LATE> <FINAL> type? initializedIdentifierList
    |    <STATIC> <LATE>? varOrType initializedIdentifierList
    |    <COVARIANT> <LATE> <FINAL> type? identifierList
    |    <COVARIANT> <LATE>? varOrType initializedIdentifierList
    |    <LATE>? (<FINAL> type? | varOrType) initializedIdentifierList
    |    redirectingFactoryConstructorSignature
    |    constantConstructorSignature (redirection | initializers)?
    |    constructorSignature (redirection | initializers)?
    ;

staticFinalDeclarationList
    :    staticFinalDeclaration ("," staticFinalDeclaration)*
    ;

staticFinalDeclaration
    :    identifier "=" expression
    ;

operatorSignature
    :    type? <OPERATOR> operator formalParameterList
    ;

operator
    :    "~"
    |    binaryOperator
    |    "[" "]"
    |    "[" "]" "="
    ;

binaryOperator
    :    multiplicativeOperator
    |    additiveOperator
    |    shiftOperator
    |    relationalOperator
    |    "=="
    |    bitwiseOperator
    ;

getterSignature
    :    type? <GET> identifier
    ;

setterSignature
    :    type? <SET> identifier formalParameterList
    ;

constructorSignature
    :    constructorName formalParameterList
    ;

constructorName
    :    typeIdentifier ("." (identifier | <NEW>))?
    ;

redirection
    :    ":" <THIS> ("." (identifier | <NEW>))? arguments
    ;

initializers
    :    ":" initializerListEntry ("," initializerListEntry)*
    ;

initializerListEntry
    :    <SUPER> arguments
    |    <SUPER> "." (identifier | <NEW>) arguments
    |    fieldInitializer
    |    assertion
    ;

fieldInitializer
    :    (<THIS> ".")? identifier "=" initializerExpression
    ;

initializerExpression
    :    conditionalExpression
    |    cascade
    ;

factoryConstructorSignature
    :    <CONST>? <FACTORY> constructorName formalParameterList
    ;

redirectingFactoryConstructorSignature
    :    <CONST>? <FACTORY> constructorName formalParameterList "="
         constructorDesignation
    ;

constantConstructorSignature
    :    <CONST> constructorName formalParameterList
    ;

mixinApplication
    :    typeNotVoidNotFunction mixins interfaces?
    ;

enumType
    :    <ENUM> typeIdentifier typeParameters? mixins? interfaces? <LBRACE>
         enumEntry ("," enumEntry)* (",")?
         (";" (metadata classMemberDefinition)*)?
         <RBRACE>
    ;

enumEntry
    :    metadata identifier argumentPart?
    |    metadata identifier typeArguments? "." identifier arguments
    ;

typeParameter
    :    metadata typeIdentifier (<EXTENDS> typeNotVoid)?
    ;

typeParameters
    :    "<" typeParameter ("," typeParameter)* ">"
    ;

metadata
    :    ("@" metadatum)*
    ;

metadatum
    :    constructorDesignation arguments
    |    identifier
    |    qualifiedName
    ;

expression
    :    functionExpression
    |    throwExpression
    |    assignableExpression assignmentOperator expression
    |    conditionalExpression
    |    cascade
    ;

expressionWithoutCascade
    :    functionExpressionWithoutCascade
    |    throwExpressionWithoutCascade
    |    assignableExpression assignmentOperator expressionWithoutCascade
    |    conditionalExpression
    ;

expressionList
    :    expression ("," expression)*
    ;

primary
    :    thisExpression
    |    <SUPER> unconditionalAssignableSelector
    |    constObjectExpression
    |    newExpression
    |    constructorInvocation
    |    functionPrimary
    |    "(" expression ")"
    |    literal
    |    identifier
    |    constructorTearoff
    ;

constructorInvocation
    :    typeName typeArguments "." <NEW> arguments
    |    typeName "." <NEW> arguments
    ;

literal
    :    nullLiteral
    |    booleanLiteral
    |    numericLiteral
    |    stringLiteral
    |    symbolLiteral
    |    setOrMapLiteral
    |    listLiteral
    ;

nullLiteral
    :    <NULL>
    ;

numericLiteral
    :    <NUMBER>
    |    <HEX_NUMBER>
    ;

booleanLiteral
    :    <TRUE>
    |    <FALSE>
    ;

stringLiteral
    :    (multiLineString | singleLineString)+
    ;


stringLiteralWithoutInterpolation
    :    singleStringWithoutInterpolation+
    ;

setOrMapLiteral
    : <CONST>? typeArguments? <LBRACE> elements? <RBRACE>
    ;

listLiteral
    : <CONST>? typeArguments? "[" elements? "]"
    ;

elements
    : element ("," element)* ","?
    ;

element
    : expressionElement
    | mapElement
    | spreadElement
    | ifElement
    | forElement
    ;

expressionElement
    : expression
    ;

mapElement
    : expression ":" expression
    ;

spreadElement
    : ("..." | "...?") expression
    ;

ifElement
    : <IF> "(" expression ")" element (<ELSE> element)?
    ;

forElement
    : <AWAIT>? <FOR> "(" forLoopParts ")" element
    ;

constructorTearoff
    :    typeName typeArguments? "." <NEW>
    ;

throwExpression
    :    <THROW> expression
    ;

throwExpressionWithoutCascade
    :    <THROW> expressionWithoutCascade
    ;

functionExpression
    :    formalParameterPart functionExpressionBody
    ;

functionExpressionBody
    :    "=>" /* TODO: { startNonAsyncFunction(); }*/ expression /* TODO: { endFunction(); }*/
    |    <ASYNC> "=>" /* TODO: { startAsyncFunction(); }*/ expression /* TODO: { endFunction(); }*/
    ;

functionExpressionBodyPrefix
    :    <ASYNC>? "=>"
    ;

functionExpressionWithoutCascade
    :    formalParameterPart functionExpressionWithoutCascadeBody
    ;

functionExpressionWithoutCascadeBody
    :    "=>" /* TODO: { startNonAsyncFunction(); }*/
         expressionWithoutCascade /* TODO: { endFunction(); }*/
    |    <ASYNC> "=>" /* TODO: { startAsyncFunction(); }*/
         expressionWithoutCascade /* TODO: { endFunction(); }*/
    ;

functionPrimary
    :    formalParameterPart functionPrimaryBody
    ;

functionPrimaryBody
    :    /* TODO: { startNonAsyncFunction(); }*/ block /* TODO: { endFunction(); }*/
    |    (<ASYNC> | <ASYNC> "*" | <SYNC> "*")
         /* TODO: { startAsyncFunction(); }*/ block /* TODO: { endFunction(); }*/
    ;

functionPrimaryBodyPrefix
    : (<ASYNC> | <ASYNC> "*" | <SYNC> "*")? <LBRACE>
    ;

thisExpression
    :    <THIS>
    ;

newExpression
    :    <NEW> constructorDesignation arguments
    ;

constObjectExpression
    :    <CONST> constructorDesignation arguments
    ;

arguments
    :    "(" (argumentList ","?)? ")"
    ;

argumentList
    :    namedArgument ("," namedArgument)*
    |    expressionList ("," namedArgument)*
    ;

namedArgument
    :    label expression
    ;

cascade
    :     cascade ".." cascadeSection
    |     conditionalExpression ("?.." | "..") cascadeSection
    ;

cascadeSection
    :    cascadeSelector cascadeSectionTail
    ;

cascadeSelector
    :    "[" expression "]"
    |    identifier
    ;

cascadeSectionTail
    :    cascadeAssignment
    |    selector* (assignableSelector cascadeAssignment)?
    ;

cascadeAssignment
    :    assignmentOperator expressionWithoutCascade
    ;

assignmentOperator
    :    "="
    |    compoundAssignmentOperator
    ;

compoundAssignmentOperator
    :    "*="
    |    "/="
    |    "~/="
    |    "%="
    |    "+="
    |    "-="
    |    "<<="
    |    ">" ">" ">" "="
    |    ">" ">" "="
    |    "&="
    |    "^="
    |    "|="
    |    "??="
    ;

conditionalExpression
    :    ifNullExpression
         ("?" expressionWithoutCascade ":" expressionWithoutCascade)?
    ;

ifNullExpression
    :    logicalOrExpression ("??" logicalOrExpression)*
    ;

logicalOrExpression
    :    logicalAndExpression ("||" logicalAndExpression)*
    ;

logicalAndExpression
    :    equalityExpression ("&&" equalityExpression)*
    ;

equalityExpression
    :    relationalExpression (equalityOperator relationalExpression)?
    |    <SUPER> equalityOperator relationalExpression
    ;

equalityOperator
    :    "=="
    |    "!="
    ;

relationalExpression
    :    bitwiseOrExpression
         (typeTest | typeCast | relationalOperator bitwiseOrExpression)?
    |    <SUPER> relationalOperator bitwiseOrExpression
    ;

relationalOperator
    :    ">" "="
    |    ">"
    |    "<="
    |    "<"
    ;

bitwiseOrExpression
    :    bitwiseXorExpression ("|" bitwiseXorExpression)*
    |    <SUPER> ("|" bitwiseXorExpression)+
    ;

bitwiseXorExpression
    :    bitwiseAndExpression ("^" bitwiseAndExpression)*
    |    <SUPER> ("^" bitwiseAndExpression)+
    ;

bitwiseAndExpression
    :    shiftExpression ("&" shiftExpression)*
    |    <SUPER> ("&" shiftExpression)+
    ;

bitwiseOperator
    :    "&"
    |    "^"
    |    "|"
    ;

shiftExpression
    :    additiveExpression (shiftOperator additiveExpression)*
    |    <SUPER> (shiftOperator additiveExpression)+
    ;

shiftOperator
    :    "<<"
    |    ">" ">" ">"
    |    ">" ">"
    ;

additiveExpression
    :    multiplicativeExpression (additiveOperator multiplicativeExpression)*
    |    <SUPER> (additiveOperator multiplicativeExpression)+
    ;

additiveOperator
    :    "+"
    |    "-"
    ;

multiplicativeExpression
    :    unaryExpression (multiplicativeOperator unaryExpression)*
    |    <SUPER> (multiplicativeOperator unaryExpression)+
    ;

multiplicativeOperator
    :    "*"
    |    "/"
    |    "%"
    |    "~/"
    ;

unaryExpression
    :    prefixOperator unaryExpression
    |    awaitExpression
    |    postfixExpression
    |    (minusOperator | tildeOperator) <SUPER>
    |    incrementOperator assignableExpression
    ;

prefixOperator
    :    minusOperator
    |    negationOperator
    |    tildeOperator
    ;

minusOperator
    :    "-"
    ;

negationOperator
    :    "!"
    ;

tildeOperator
    :    "~"
    ;

awaitExpression
    :    <AWAIT> unaryExpression
    ;

postfixExpression
    :    assignableExpression postfixOperator
    |    primary selector*
    ;

postfixOperator
    :    incrementOperator
    ;

selector
    :    "!"
    |    assignableSelector
    |    argumentPart
    |    typeArguments
    ;

argumentPart
    :    typeArguments? arguments
    ;

incrementOperator
    :    "++"
    |    "--"
    ;

assignableExpression
    :    <SUPER> unconditionalAssignableSelector
    |    primary assignableSelectorPart
    |    identifier
    ;

assignableSelectorPart
    :    selector* assignableSelector
    ;

unconditionalAssignableSelector
    :    "[" expression "]"
    |    "." identifier
    ;

assignableSelector
    :    unconditionalAssignableSelector
    |    "?." identifier
    |    "?" "[" expression "]"
    ;

identifierNotFUNCTION
    :    <IDENTIFIER>
    |    builtInIdentifier
    |    <ASYNC> 
    |    <HIDE> 
    |    <OF> 
    |    <ON> 
    |    <SHOW> 
    |    <SYNC> 
    |    /* TODO: { asyncEtcPredicate(getCurrentToken().getType())? }*/ (<AWAIT>|<YIELD>)
    ;

identifier
    :    identifierNotFUNCTION
    |    <FUNCTION> 
    ;

qualifiedName
    :    typeIdentifier "." (identifier | <NEW>)
    |    typeIdentifier "." typeIdentifier "." (identifier | <NEW>)
    ;

typeIdentifier
    :    <IDENTIFIER>
    |    <DYNAMIC> 
    |    <ASYNC> 
    |    <HIDE> 
    |    <OF> 
    |    <ON> 
    |    <SHOW> 
    |    <SYNC> 
    |    /* TODO: { asyncEtcPredicate(getCurrentToken().getType()) }?*/ (<AWAIT>|<YIELD>)
    ;

typeTest
    :    isOperator typeNotVoid
    ;

isOperator
    :    <IS> "!"?
    ;

typeCast
    :    asOperator typeNotVoid
    ;

asOperator
    :    <AS>
    ;

statements
    :    statement*
    ;

statement
    :    label* nonLabelledStatement
    ;







nonLabelledStatement
    :    block
    |    localVariableDeclaration
    |    forStatement
    |    whileStatement
    |    doStatement
    |    switchStatement
    |    ifStatement
    |    rethrowStatement
    |    tryStatement
    |    breakStatement
    |    continueStatement
    |    returnStatement
    |    localFunctionDeclaration
    |    assertStatement
    |    yieldStatement
    |    yieldEachStatement
    |    expressionStatement
    ;

expressionStatement
    :    expression? ";"
    ;

localVariableDeclaration
    :    metadata initializedVariableDeclaration ";"
    ;

initializedVariableDeclaration
    :    declaredIdentifier ("=" expression)? ("," initializedIdentifier)*
    ;

localFunctionDeclaration
    :    metadata functionSignature functionBody
    ;

ifStatement
    :    <IF> "(" expression ")" statement (<ELSE> statement)?
    ;

forStatement
    :    <AWAIT>? <FOR> "(" forLoopParts ")" statement
    ;

forLoopParts
    :    metadata declaredIdentifier <IN> expression
    |    metadata identifier <IN> expression
    |    forInitializerStatement expression? ";" expressionList?
    ;



forInitializerStatement
    :    localVariableDeclaration
    |    expression? ";"
    ;

whileStatement
    :    <WHILE> "(" expression ")" statement
    ;

doStatement
    :    <DO> statement <WHILE> "(" expression ")" ";"
    ;

switchStatement
    :    <SWITCH> "(" expression ")" <LBRACE> switchCase* defaultCase? <RBRACE>
    ;

switchCase
    :    label* <CASE> expression ":" statements
    ;

defaultCase
    :    label* <DEFAULT> ":" statements
    ;

rethrowStatement
    :    <RETHROW> ";"
    ;

tryStatement
    :    <TRY> block (onParts finallyPart? | finallyPart)
    ;

onPart
    :    catchPart block
    |    <ON> typeNotVoid catchPart? block
    ;

onParts
    :    onPart onParts
    |    onPart
    ;

catchPart
    :    <CATCH> "(" identifier ("," identifier)? ")"
    ;

finallyPart
    :    <FINALLY> block
    ;

returnStatement
    :    <RETURN> expression? ";"
    ;

label
    :    identifier ":"
    ;

breakStatement
    :    <BREAK> identifier? ";"
    ;

continueStatement
    :    <CONTINUE> identifier? ";"
    ;

yieldStatement
    :    <YIELD> expression ";"
    ;

yieldEachStatement
    :    <YIELD> "*" expression ";"
    ;

assertStatement
    :    assertion ";"
    ;

assertion
    :    <ASSERT> "(" expression ("," expression)? ","? ")"
    ;

libraryName
    :    metadata <LIBRARY> dottedIdentifierList ";"
    ;

dottedIdentifierList
    :    identifier ("." identifier)*
    ;

importOrExport
    :    libraryImport
    |    libraryExport
    ;

libraryImport
    :    metadata importSpecification
    ;

importSpecification
    :    <IMPORT> configurableUri (<DEFERRED>? <AS> identifier)? combinator* ";"
    ;

combinator
    :    <SHOW> identifierList
    |    <HIDE> identifierList
    ;

identifierList
    :    identifier ("," identifier)*
    ;

libraryExport
    :    metadata <EXPORT> uri combinator* ";"
    ;

partDirective
    :    metadata <PART> uri ";"
    ;

partHeader
    :    metadata <PART> <OF> (dottedIdentifierList | uri)";"
    ;

partDeclaration
    :    partHeader topLevelDefinition* 
    ;



uri
    :    stringLiteralWithoutInterpolation
    ;

configurableUri
    :    uri configurationUri*
    ;

configurationUri
    :    <IF> "(" uriTest ")" uri
    ;

uriTest
    :    dottedIdentifierList ("==" stringLiteral)?
    ;

type
    :    functionType "?"?
    |    typeNotFunction
    ;

typeNotVoid
    :    functionType "?"?
    |    typeNotVoidNotFunction
    ;

typeNotFunction
    :    typeNotVoidNotFunction
    |    <VOID>
    ;

typeNotVoidNotFunction
    :    typeName typeArguments? "?"?
    |    <FUNCTION> "?"?
    ;

typeName
    :    typeIdentifier ("." typeIdentifier)?
    ;

typeArguments
    :    "<" typeList ">"
    ;

typeList
    :    type ("," type)*
    ;

typeNotVoidNotFunctionList
    :    typeNotVoidNotFunction ("," typeNotVoidNotFunction)*
    ;

typeAlias
    :    <TYPEDEF> typeIdentifier typeParameters? "=" type ";"
    |    <TYPEDEF> functionTypeAlias
    ;

functionTypeAlias
    :    functionPrefix formalParameterPart ";"
    ;

functionPrefix
    :    type identifier
    |    identifier
    ;

functionTypeTail
    :    <FUNCTION> typeParameters? parameterTypeList
    ;

functionTypeTails
    :    functionTypeTail "?"? functionTypeTails
    |    functionTypeTail
    ;

functionType
    :    functionTypeTails
    |    typeNotFunction functionTypeTails
    ;

parameterTypeList
    :    "(" ")"
    |    "(" normalParameterTypes "," optionalParameterTypes ")"
    |    "(" normalParameterTypes ","? ")"
    |    "(" optionalParameterTypes ")"
    ;

normalParameterTypes
    :    normalParameterType ("," normalParameterType)*
    ;

normalParameterType
    :    metadata typedIdentifier
    |    metadata type
    ;

optionalParameterTypes
    :    optionalPositionalParameterTypes
    |    namedParameterTypes
    ;

optionalPositionalParameterTypes
    :    "[" normalParameterTypes ","? "]"
    ;

namedParameterTypes
    :    <LBRACE> namedParameterType ("," namedParameterType)* ","? <RBRACE>
    ;

namedParameterType
    :    metadata <REQUIRED>? typedIdentifier
    ;

typedIdentifier
    :    type identifier
    ;

constructorDesignation
    :    typeIdentifier
    |    qualifiedName
    |    typeName typeArguments ("." (identifier | <NEW>))?
    ;

symbolLiteral
    :    "#" (operator | (identifier ("." identifier)*) | <VOID>)
    ;


singleStringWithoutInterpolation
    :    <RAW_SINGLE_LINE_STRING>
    |    <RAW_MULTI_LINE_STRING>
    |    <SINGLE_LINE_STRING_DQ_BEGIN_END>
    |    <SINGLE_LINE_STRING_SQ_BEGIN_END>
    |    <MULTI_LINE_STRING_DQ_BEGIN_END>
    |    <MULTI_LINE_STRING_SQ_BEGIN_END>
    ;

singleLineString
    :    <RAW_SINGLE_LINE_STRING>
    |    <SINGLE_LINE_STRING_SQ_BEGIN_END>
    |    <SINGLE_LINE_STRING_SQ_BEGIN_MID> expression
         (<SINGLE_LINE_STRING_SQ_MID_MID> expression)*
         <SINGLE_LINE_STRING_SQ_MID_END>
    |    <SINGLE_LINE_STRING_DQ_BEGIN_END>
    |    <SINGLE_LINE_STRING_DQ_BEGIN_MID> expression
         (<SINGLE_LINE_STRING_DQ_MID_MID> expression)*
         <SINGLE_LINE_STRING_DQ_MID_END>
    ;

multiLineString
    :    <RAW_MULTI_LINE_STRING>
    |    <MULTI_LINE_STRING_SQ_BEGIN_END>
    |    <MULTI_LINE_STRING_SQ_BEGIN_MID> expression
         (<MULTI_LINE_STRING_SQ_MID_MID> expression)*
         <MULTI_LINE_STRING_SQ_MID_END>
    |    <MULTI_LINE_STRING_DQ_BEGIN_END>
    |    <MULTI_LINE_STRING_DQ_BEGIN_MID> expression
         (<MULTI_LINE_STRING_DQ_MID_MID> expression)*
         <MULTI_LINE_STRING_DQ_MID_END>
    ;

reservedWord
    :    <ASSERT>
    |    <BREAK>
    |    <CASE>
    |    <CATCH>
    |    <CLASS>
    |    <CONST>
    |    <CONTINUE>
    |    <DEFAULT>
    |    <DO>
    |    <ELSE>
    |    <ENUM>
    |    <EXTENDS>
    |    <FALSE>
    |    <FINAL>
    |    <FINALLY>
    |    <FOR>
    |    <IF>
    |    <IN>
    |    <IS>
    |    <NEW>
    |    <NULL>
    |    <RETHROW>
    |    <RETURN>
    |    <SUPER>
    |    <SWITCH>
    |    <THIS>
    |    <THROW>
    |    <TRUE>
    |    <TRY>
    |    <VAR>
    |    <VOID>
    |    <WHILE>
    |    <WITH>
    ;

builtInIdentifier
    :    <ABSTRACT>
    |    <AS>
    |    <COVARIANT>
    |    <DEFERRED>
    |    <DYNAMIC>
    |    <EXPORT>
    |    <EXTENSION>
    |    <EXTERNAL>
    |    <FACTORY>
    |    <FUNCTION>
    |    <GET>
    |    <IMPLEMENTS>
    |    <IMPORT>
    |    <INTERFACE>
    |    <LATE>
    |    <LIBRARY>
    |    <OPERATOR>
    |    <MIXIN>
    |    <PART>
    |    <REQUIRED>
    |    <SET>
    |    <STATIC>
    |    <TYPEDEF>
    ;



<LETTER>
    :    [a-z]
    |    [A-Z]
    ;

<DIGIT>
    :    [0-9]
    ;

<EXPONENT>
    :    (e | E) (\+ | \-)? <DIGIT>+
    ;

<HEX_DIGIT>
    :    (a | b | c | d | e | f)
    |    (A | B | C | D | E | F)
    |    <DIGIT>
    ;



<ASSERT>
    :    assert
    ;

<BREAK>
    :    break
    ;

<CASE>
    :    case
    ;

<CATCH>
    :    catch
    ;

<CLASS>
    :    class
    ;

<CONST>
    :    const
    ;

<CONTINUE>
    :    continue
    ;

<DEFAULT>
    :    default
    ;

<DO>
    :    do
    ;

<ELSE>
    :    else
    ;

<ENUM>
    :    enum
    ;

<EXTENDS>
    :    extends
    ;

<FALSE>
    :    false
    ;

<FINAL>
    :    final
    ;

<FINALLY>
    :    finally
    ;

<FOR>
    :    for
    ;

<IF>
    :    if
    ;

<IN>
    :    in
    ;

<IS>
    :    is
    ;

<NEW>
    :    new
    ;

<NULL>
    :    null
    ;

<RETHROW>
    :    rethrow
    ;

<RETURN>
    :    return
    ;

<SUPER>
    :    super
    ;

<SWITCH>
    :    switch
    ;

<THIS>
    :    this
    ;

<THROW>
    :    throw
    ;

<TRUE>
    :    true
    ;

<TRY>
    :    try
    ;

<VAR>
    :    var
    ;

<VOID>
    :    void
    ;

<WHILE>
    :    while
    ;

<WITH>
    :    with
    ;

<ABSTRACT>
    :    abstract
    ;

<AS>
    :    as
    ;

<COVARIANT>
    :    covariant
    ;

<DEFERRED>
    :    deferred
    ;

<DYNAMIC>
    :    dynamic
    ;

<EXPORT>
    :    export
    ;

<EXTENSION>
    :    extension
    ;

<EXTERNAL>
    :    external
    ;

<FACTORY>
    :    factory
    ;

<FUNCTION>
    :    Function
    ;

<GET>
    :    get
    ;

<IMPLEMENTS>
    :    implements
    ;

<IMPORT>
    :    import
    ;

<INTERFACE>
    :    interface
    ;

<LATE>
    :    late
    ;

<LIBRARY>
    :    library
    ;

<OPERATOR>
    :    operator
    ;

<MIXIN>
    :    mixin
    ;

<PART>
    :    part
    ;

<REQUIRED>
    :    required
    ;

<SET>
    :    set
    ;

<STATIC>
    :    static
    ;

<TYPEDEF>
    :    typedef
    ;



<AWAIT>
    :    await
    ;

<YIELD>
    :    yield
    ;



<ASYNC>
    :    async
    ;

<HIDE>
    :    hide
    ;

<OF>
    :    of
    ;

<ON>
    :    on
    ;

<SHOW>
    :    show
    ;

<SYNC>
    :    sync
    ;



<NUMBER>
    :    <DIGIT>+ \. <DIGIT>+ <EXPONENT>?
    |    <DIGIT>+ <EXPONENT>?
    |    \. <DIGIT>+ <EXPONENT>?
    ;

<HEX_NUMBER>
    :    0x <HEX_DIGIT>+
    |    0X <HEX_DIGIT>+
    ;

<RAW_SINGLE_LINE_STRING>
    :    r \' ([^\'\r\n])* \'
    |    r \" ([^\"\r\n])* \"
    ;

<RAW_MULTI_LINE_STRING>
    :    r \"\"\" (.)*/* TODO: ? */ \"\"\"
    |    r \'\'\' (.)*/* TODO: ? */ \'\'\'
    ;

<SIMPLE_STRING_INTERPOLATION>
    :    \$ <IDENTIFIER_NO_DOLLAR>
    ;

<ESCAPE_SEQUENCE>
    :    \\n
    |    \\r
    |    \\b
    |    \\t
    |    \\v
    |    \\x <HEX_DIGIT> <HEX_DIGIT>
    |    \\u <HEX_DIGIT> <HEX_DIGIT> <HEX_DIGIT> <HEX_DIGIT>
    |    \\u\{ <HEX_DIGIT_SEQUENCE> \}
    ;

<HEX_DIGIT_SEQUENCE>
    :    <HEX_DIGIT> <HEX_DIGIT>? <HEX_DIGIT>?
         <HEX_DIGIT>? <HEX_DIGIT>? <HEX_DIGIT>?
    ;

<STRING_CONTENT_COMMON>
    :    [^\\\'\"\$\r\n]
    |    <ESCAPE_SEQUENCE>
    |    \\ [^nrbtvxu\r\n]
    |    <SIMPLE_STRING_INTERPOLATION>
    ;

<STRING_CONTENT_SQ>
    :    <STRING_CONTENT_COMMON>
    |    \"
    ;

<SINGLE_LINE_STRING_SQ_BEGIN_END>
    :    \' <STRING_CONTENT_SQ>* \'
    ;

<SINGLE_LINE_STRING_SQ_BEGIN_MID>
    :    \' <STRING_CONTENT_SQ>* \$\{ /* TODO: { enterBraceSingleQuote(); }*/
    ;

<SINGLE_LINE_STRING_SQ_MID_MID>
    :    /* TODO: { currentBraceLevel(BRACE_SINGLE) }*/?
         /* TODO: { exitBrace(); }*/ \} <STRING_CONTENT_SQ>* \$\{
         /* TODO: { enterBraceSingleQuote(); }*/
    ;

<SINGLE_LINE_STRING_SQ_MID_END>
    :    /* TODO: { currentBraceLevel(BRACE_SINGLE) }*/?
         /* TODO: { exitBrace(); }*/ \} <STRING_CONTENT_SQ>* \'
    ;

<STRING_CONTENT_DQ>
    :    <STRING_CONTENT_COMMON>
    |    \'
    ;

<SINGLE_LINE_STRING_DQ_BEGIN_END>
    :    \" <STRING_CONTENT_DQ>* \"
    ;

<SINGLE_LINE_STRING_DQ_BEGIN_MID>
    :    \" <STRING_CONTENT_DQ>* \$\{ /* TODO: { enterBraceDoubleQuote(); }*/
    ;

<SINGLE_LINE_STRING_DQ_MID_MID>
    :    /* TODO: { currentBraceLevel(BRACE_DOUBLE) }*/?
         /* TODO: { exitBrace(); }*/ \} <STRING_CONTENT_DQ>* \$\{
         /* TODO: { enterBraceDoubleQuote(); }*/
    ;

<SINGLE_LINE_STRING_DQ_MID_END>
    :    /* TODO: { currentBraceLevel(BRACE_DOUBLE) }*/?
         /* TODO: { exitBrace(); }*/ \} <STRING_CONTENT_DQ>* \"
    ;

<QUOTES_SQ>
    :
    |    \'
    |    \'\'
    ;





<STRING_CONTENT_TSQ>
    :    <QUOTES_SQ>
         (<STRING_CONTENT_COMMON> | \" | \r | \n | \\\r | \\\n)
    ;

<MULTI_LINE_STRING_SQ_BEGIN_END>
    :    \'\'\' <STRING_CONTENT_TSQ>* \'\'\'
    ;

<MULTI_LINE_STRING_SQ_BEGIN_MID>
    :    \'\'\' <STRING_CONTENT_TSQ>* <QUOTES_SQ> \$\{
         /* TODO: { enterBraceThreeSingleQuotes(); }*/
    ;

<MULTI_LINE_STRING_SQ_MID_MID>
    :    /* TODO: { currentBraceLevel(BRACE_THREE_SINGLE) }*/?
         /* TODO: { exitBrace(); }*/ \} <STRING_CONTENT_TSQ>* <QUOTES_SQ> \$\{
         /* TODO: { enterBraceThreeSingleQuotes(); }*/
    ;

<MULTI_LINE_STRING_SQ_MID_END>
    :    /* TODO: { currentBraceLevel(BRACE_THREE_SINGLE) }*/?
         /* TODO: { exitBrace(); }*/ \} <STRING_CONTENT_TSQ>* \'\'\'
    ;

<QUOTES_DQ>
    :
    |    \"
    |    \"\"
    ;




<STRING_CONTENT_TDQ>
    :    <QUOTES_DQ>
         (<STRING_CONTENT_COMMON> | \' | \r | \n | \\\r | \\\n)
    ;

<MULTI_LINE_STRING_DQ_BEGIN_END>
    :    \"\"\" <STRING_CONTENT_TDQ>* \"\"\"
    ;

<MULTI_LINE_STRING_DQ_BEGIN_MID>
    :    \"\"\" <STRING_CONTENT_TDQ>* <QUOTES_DQ> \$\{
         /* TODO: { enterBraceThreeDoubleQuotes(); }*/
    ;

<MULTI_LINE_STRING_DQ_MID_MID>
    :    /* TODO: { currentBraceLevel(BRACE_THREE_DOUBLE) }*/?
         /* TODO: { exitBrace(); }*/ \} <STRING_CONTENT_TDQ>* <QUOTES_DQ> \$\{
         /* TODO: { enterBraceThreeDoubleQuotes(); }*/
    ;

<MULTI_LINE_STRING_DQ_MID_END>
    :    /* TODO: { currentBraceLevel(BRACE_THREE_DOUBLE) }*/?
         /* TODO: { exitBrace(); }*/ \} <STRING_CONTENT_TDQ>* \"\"\"
    ;

<LBRACE>
    :    \{ /* TODO: { enterBrace(); }*/
    ;

<RBRACE>
    :    /* TODO: { currentBraceLevel(BRACE_NORMAL) }*/? /* TODO: { exitBrace(); }*/ \}
    ;

<IDENTIFIER_START_NO_DOLLAR>
    :    <LETTER>
    |    _
    ;

<IDENTIFIER_PART_NO_DOLLAR>
    :    <IDENTIFIER_START_NO_DOLLAR>
    |    <DIGIT>
    ;

<IDENTIFIER_NO_DOLLAR>
    :    <IDENTIFIER_START_NO_DOLLAR> <IDENTIFIER_PART_NO_DOLLAR>*
    ;

<IDENTIFIER_START>
    :    <IDENTIFIER_START_NO_DOLLAR>
    |    \$
    ;

<IDENTIFIER_PART>
    :    <IDENTIFIER_START>
    |    <DIGIT>
    ;

<SCRIPT_TAG>
    :    \#\! ([^\r\n])* <NEWLINE>
    ;

<IDENTIFIER>
    :    <IDENTIFIER_START> <IDENTIFIER_PART>*
    ;

<SINGLE_LINE_COMMENT>
    :    \/\/ ([^\r\n])* <NEWLINE>?
         /* TODO: { skip(); }*/
    ;

/* TODO
<MULTI_LINE_COMMENT>
    :    \/\* (<MULTI_LINE_COMMENT> | .)*? \*\/
          TODO: { skip(); }
    ;
*/

<NEWLINE>
    :    (\r | \n | \r\n)
    ;

<FEFF>
    :    \xFEFF
    ;

<WS>
    :    (\  | \t | \r | \n)+
         /* TODO: { skip(); }*/
    ;

<whitespace>
	: \s+
	;