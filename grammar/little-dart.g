/* Configuration */

%glr;
%prefix Dart ;
%suffix Node ;
%root Program ;

/* Hierarchy */
/* Use to create the class heritage hierarchy inside Pharo 
(e.g. ImportDeclaration will be a subClass of ImportExportDeclaration)
%hierarchy FunctionBody (AsyncFunctionBody);
%hierarchy Statement (ExpressionStatement LocalVariableDeclaration IfStatement ForStatement WhileStatement DoStatement SwitchCaseStatement TryStatement ReturnStatement BreakStatement ContinueStatement YieldStatement AssertStatement);
%hierarchy YieldStatement (YieldEachStatement);
  */
/*grammar Dart;*/

compilationUnit 
    : (libraryDeclaration 
    | partDeclaration 
    | expression 
    | statement) EOF ;

libraryDefinition
    :    <FEFF>? <SCRIPT_TAG>?
         libraryName?
         importOrExport*
         partDirective*
         (metadata topLevelDefinition)* {{LibraryDefinition}}
         
    ;

topLevelDefinition
    :    classDeclaration 'classDeclaration'
    |    mixinDeclaration
    |    extensionDeclaration
    |    enumType
    |    typeAlias
    |    <external> functionSignature ";"
    |    <external> getterSignature ";"
    |    <external> setterSignature ";"
    |    <external> finalVarOrType identifierList ";"
    |    getterSignature functionBody
    |    setterSignature functionBody
    |    functionSignature functionBody
    |    (<final> | <const>) type? staticFinalDeclarationList ";"
    |    <late> <final> type? initializedIdentifierList ";"
    |    <late>? varOrType identifier ("=" expression)?
         ("," initializedIdentifier)* ";"
    ;

declaredIdentifier
    :    <covariant>? finalConstVarOrType 'finalConstVarOrType' identifier 'declaredIdentifier' {{}}
    ;

finalConstVarOrType
    :    <late>? <final> type?
    |    <const> type?
    |    <late>? varOrType
    ;

finalVarOrType
    :    <final> type?
    |    varOrType
    ;

varOrType
    :    <var>
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
    :    <async>? "=>"
    |    (<async> | <async> "*" | <sync> "*")? <lbrace>
    ;

functionBody
    :    "=>" /* TODO: { startNonAsyncFunction(); }*/ expression 'expression' /* TODO: { endFunction(); }*/ ";" {{FunctionBody}}
    |    /* TODO: { startNonAsyncFunction(); }*/ block 'block' /* TODO: { endFunction(); }*/ {{FunctionBody}}
    |    <async> "=>" /* TODO: { startAsyncFunction(); }*/ expression 'expression' /* TODO: { endFunction(); }*/ ";" {{AsyncFunctionBody}}
    |    (<async> | <async> "*" | <sync> "*") /* TODO: { startAsyncFunction(); }*/ block 'block' /* TODO: { endFunction(); }*/ {{AsyncFunctionBody}}
    ;

block
    :    <lbrace> statements 'statements' <rbrace> {{Block}}
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
    :    <lbrace> defaultNamedParameter ("," defaultNamedParameter)* ","? <rbrace>
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
    :    <covariant>? type? identifierNotFUNCTION formalParameterPart "?"?
    ;

simpleFormalParameter
    :    declaredIdentifier
    |    <covariant>? identifier
    ;


fieldFormalParameter
    :    finalConstVarOrType? <this> "." identifier (formalParameterPart "?"?)?
    ;

defaultFormalParameter
    :    normalFormalParameter ("=" expression)?
    ;

defaultNamedParameter
    :    <required>? normalFormalParameter ((":" | "=") expression)?
    ;

typeWithParameters
    :    typeIdentifier 'typeIdentifier' typeParameters? {{Type}}
    ;

classDeclaration
    :    <abstract>? <clazz> typeWithParameters 'type' superclass? mixins? interfaces? <lbrace> (metadata classMemberDefinition 'classMemberDefinition')* <rbrace> {{ClassDeclaration}}
    |    <abstract>? <clazz> mixinApplicationClass
    ;

superclass
    :    <extends> typeNotVoidNotFunction
    ;

mixins
    :    <with> typeNotVoidNotFunctionList
    ;

interfaces
    :    <implements> typeNotVoidNotFunctionList
    ;

classMemberDefinition
    :    methodSignature 'methodSignature' functionBody 'functionBody' {{ClassMemberDefinition}}
    |    declaration 'declaration' ";" {{ClassMemberDefinition}}
    ;

mixinApplicationClass
    :    typeWithParameters "=" mixinApplication ";"
    ;

mixinDeclaration
    :    <mixin> typeIdentifier typeParameters?
         (<on> typeNotVoidNotFunctionList)? interfaces?
         <lbrace> (metadata mixinMemberDefinition)* <rbrace>
    ;


mixinMemberDefinition
    :    classMemberDefinition
    ;

extensionDeclaration
    :    <extension> identifier? typeParameters? <on> type
         <lbrace> (metadata extensionMemberDefinition)* <rbrace>
    ;


extensionMemberDefinition
    :    classMemberDefinition
    ;

methodSignature
    :    constructorSignature initializers
    |    factoryConstructorSignature
    |    <static>? functionSignature
    |    <static>? getterSignature
    |    <static>? setterSignature
    |    operatorSignature
    |    constructorSignature
    ;

declaration
    :    <external> factoryConstructorSignature
    |    <external> constantConstructorSignature
    |    <external> constructorSignature
    |    (<external> <static>?)? getterSignature
    |    (<external> <static>?)? setterSignature
    |    (<external> <static>?)? functionSignature
    |    <external> (<static>? finalVarOrType | <covariant> varOrType) identifierList
    |    <abstract> (finalVarOrType | <covariant> varOrType) identifierList
    |    <external>? operatorSignature
    |    <static> (<final> | <const>) type? staticFinalDeclarationList
    |    <static> <late> <final> type? initializedIdentifierList
    |    <static> <late>? varOrType initializedIdentifierList
    |    <covariant> <late> <final> type? identifierList
    |    <covariant> <late>? varOrType initializedIdentifierList
    |    <late>? (<final> type? | varOrType) initializedIdentifierList
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
    :    type? <operator> operator formalParameterList
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
    :    type? <get> identifier
    ;

setterSignature
    :    type? <set> identifier formalParameterList
    ;

constructorSignature
    :    constructorName formalParameterList
    ;

constructorName
    :    typeIdentifier ("." (identifier | <new>))?
    ;

redirection
    :    ":" <this> ("." (identifier | <new>))? arguments
    ;

initializers
    :    ":" initializerListEntry ("," initializerListEntry)*
    ;

initializerListEntry
    :    <super> arguments
    |    <super> "." (identifier | <new>) arguments
    |    fieldInitializer
    |    assertion
    ;

fieldInitializer
    :    (<this> ".")? identifier "=" initializerExpression
    ;

initializerExpression
    :    conditionalExpression
    |    cascade
    ;

factoryConstructorSignature
    :    <const>? <factory> constructorName formalParameterList
    ;

redirectingFactoryConstructorSignature
    :    <const>? <factory> constructorName formalParameterList "="
         constructorDesignation
    ;

constantConstructorSignature
    :    <const> constructorName formalParameterList
    ;

mixinApplication
    :    typeNotVoidNotFunction mixins interfaces?
    ;

enumType
    :    <enum> typeIdentifier typeParameters? mixins? interfaces? <lbrace>
         enumEntry ("," enumEntry)* (",")?
         (";" (metadata classMemberDefinition)*)?
         <rbrace>
    ;

enumEntry
    :    metadata identifier argumentPart?
    |    metadata identifier typeArguments? "." identifier arguments
    ;

typeParameter
    :    metadata typeIdentifier (<extends> typeNotVoid)?
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
    :    assignableExpressionWithOperator 
    |    conditionalExpression
    |    cascade
    |    throwExpression
    ;



assignableExpressionWithOperator
    :   assignableExpression 'assignableExpression' assignmentOperator 'assignmentOperator' expression 'assigmnentExpression' {{}}
    ;

expressionWithoutCascade
    :    assignableExpressionWithoutCascadeWithOperator 
    |    functionExpressionWithoutCascade
    |    conditionalExpression
    |    throwExpressionWithoutCascade
    ;

assignableExpressionWithoutCascadeWithOperator
    :   assignableExpression 'assignableExpression' assignmentOperator 'assignmentOperator' expressionWithoutCascade 'expressionWithoutCascade' {{}}
    ;

expressionList
    :    expression ("," expression)*
    ;

primary
    :    thisExpression
    |    <super> unconditionalAssignableSelector
    |    <super> argumentPart
    |    functionExpression
    |    literal
    |    identifier
    |    newExpression
    |    constObjectExpression
    |    constructorInvocation
    |    functionPrimary
    |    "(" expression ")"
    |    constructorTearoff
    ;

constructorInvocation
    :    typeName typeArguments "." <new> arguments
    |    typeName "." <new> arguments
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
    :    <null>
    ;

numericLiteral
    :    <number>
    |    <HEX_NUMBER>
    ;

booleanLiteral
    :    <true>
    |    <false>
    ;

stringLiteral
    :    (multiLineString | singleLineString)+
    ;


stringLiteralWithoutInterpolation
    :    singleStringWithoutInterpolation+
    ;

setOrMapLiteral
    : <const>? typeArguments? <lbrace> elements? <rbrace>
    ;

listLiteral
    : <const>? typeArguments? "[" elements? "]"
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
    : expressions
    ;

mapElement
    : expression ":" expression
    ;

spreadElement
    : ("..." | "...?") expression
    ;

ifElement
    : <if> "(" expression ")" element (<else> element)?
    ;

forElement
    : <await>? <for> "(" forLoopParts ")" element
    ;

constructorTearoff
    :    typeName typeArguments? "." <new>
    ;

throwExpression
    :    <throw> expression 'throwExpression' {{}}
    ;

throwExpressionWithoutCascade
    :    <throw> expressionWithoutCascade
    ;

functionExpression
    :    formalParameterPart 'formalParameterPart' functionExpressionBody 'functionExpressionBody' {{}}
    ;

functionExpressionBody
    :    "=>" /* TODO: { startNonAsyncFunction(); }*/ expression /* TODO: { endFunction(); }*/
    |    <async> "=>" /* TODO: { startAsyncFunction(); }*/ expression /* TODO: { endFunction(); }*/
    ;

functionExpressionBodyPrefix
    :    <async>? "=>"
    ;

functionExpressionWithoutCascade
    :    formalParameterPart functionExpressionWithoutCascadeBody
    ;

functionExpressionWithoutCascadeBody
    :    "=>" /* TODO: { startNonAsyncFunction(); }*/
         expressionWithoutCascade /* TODO: { endFunction(); }*/
    |    <async> "=>" /* TODO: { startAsyncFunction(); }*/
         expressionWithoutCascade /* TODO: { endFunction(); }*/
    ;

functionPrimary
    :    formalParameterPart functionPrimaryBody
    ;

functionPrimaryBody
    :    /* TODO: { startNonAsyncFunction(); }*/ block /* TODO: { endFunction(); }*/
    |    (<async> | <async> "*" | <sync> "*")
         /* TODO: { startAsyncFunction(); }*/ block /* TODO: { endFunction(); }*/
    ;

functionPrimaryBodyPrefix
    : (<async> | <async> "*" | <sync> "*")? <lbrace>
    ;

thisExpression
    :    <this> {{}}
    ;

newExpression
    :    <new>? constructorDesignation 'constructorDesignation' arguments 'constructorArguments' {{}}
    ;

constObjectExpression
    :    <const> constructorDesignation arguments
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
    :     cascade 'cascade' ".." cascadeSection 'cascadeSection' {{}}
    |     conditionalExpression 'conditionalExpression' ("?.." | "..") cascadeSection 'cascadeSection' {{}}
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
    :    "=" 'equalsSymbol' {{}}
    |    compoundAssignmentOperator 'compoundAssignmentOperator' {{}}
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
    :    ifNullExpression 'ifNullExpression' ( "?" expressionWithoutCascade 'firstExpressionWithoutCascade' ":" expressionWithoutCascade 'secondExpressionWithoutCascade' )? {{}}
    ;


ifNullExpression
    :    logicalOrExpression 'logicalOrExpression' ("??" logicalOrExpression 'logicalOrExpression')* {{IfNullExpression}}
    ;

logicalOrExpression
    :    logicalAndExpression 'logicalAndExpression' ("||" logicalAndExpression 'logicalAndExpression')* {{}}
    ;

logicalAndExpression
    :    equalityExpression 'equalityExpression' ("&&" equalityExpression 'equalityExpression')* {{}}
    ;

equalityExpression
    :    relationalExpression 'relationalExpression' (equalityOperator 'equalityOperator' relationalExpression 'relationalExpression')? {{EqualityExpression}}
    |    <super> equalityOperator 'equalityOperator' relationalExpression 'relationalExpression' {{EqualityExpression}}
    ;

equalityOperator
    :    "=="
    |    "!="
    ;

relationalExpression
    :    bitwiseOrExpression
         (typeTest | typeCast | relationalOperator bitwiseOrExpression)? 
    |    <super> relationalOperator bitwiseOrExpression 
    ;

relationalOperator
    :    ">" "="
    |    ">"
    |    "<="
    |    "<"
    ;

bitwiseOrExpression
    :    bitwiseXorExpression ("|" bitwiseXorExpression)*
    |    <super> ("|" bitwiseXorExpression)+
    ;

bitwiseXorExpression
    :    bitwiseAndExpression ("^" bitwiseAndExpression)*
    |    <super> ("^" bitwiseAndExpression)+
    ;

bitwiseAndExpression
    :    shiftExpression ("&" shiftExpression)*
    |    <super> ("&" shiftExpression)+
    ;

bitwiseOperator
    :    "&"
    |    "^"
    |    "|"
    ;

shiftExpression
    :    additiveExpression (shiftOperator additiveExpression)*
    |    <super> (shiftOperator additiveExpression)+
    ;

shiftOperator
    :    "<<"
    |    ">" ">" ">"
    |    ">" ">"
    ;

additiveExpression
    :    multiplicativeExpression (additiveOperator multiplicativeExpression)*
    |    <super> (additiveOperator multiplicativeExpression)+
    ;

additiveOperator
    :    "+"
    |    "-"
    ;

multiplicativeExpression
    :    unaryExpression (multiplicativeOperator unaryExpression)*
    |    <super> (multiplicativeOperator unaryExpression)+
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
    |    (minusOperator | tildeOperator) <super>
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
    :    <await> unaryExpression
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
    :    primary 'primary' assignableSelectorPart 'assignableSelectorPart' {{}}
    |    <super> unconditionalAssignableSelector 'unconditionalAssignableSelector' {{}}
    |    identifier 'identifier' {{}}
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
    |    <async> 
    |    <hide> 
    |    <of> 
    |    <on> 
    |    <show> 
    |    <sync> 
    |    /* TODO: { asyncEtcPredicate(getCurrentToken().getType())? }*/ (<await>|<yield>)
    ;

identifier
    :    identifierNotFUNCTION 'idNotFunction' {{}}
    |    <function> 'function' {{}}
    ;

qualifiedName
    :    typeIdentifier "." (identifier | <new>)
    |    typeIdentifier "." typeIdentifier "." (identifier | <new>)
    ;

typeIdentifier
    :    <IDENTIFIER>
    |    <dynamic> 
    |    <async> 
    |    <hide> 
    |    <of> 
    |    <on> 
    |    <show> 
    |    <sync> 
    |    /* TODO: { asyncEtcPredicate(getCurrentToken().getType()) }?*/ (<await>|<yield>)
    ;

typeTest
    :    isOperator typeNotVoid
    ;

isOperator
    :    <is> "!"?
    ;

typeCast
    :    asOperator typeNotVoid
    ;

asOperator
    :    <as>
    ;

statements
    :    statement 'statementsSet'* {{SequentialStatements}}
    ;

statement
    :    label 'label'* nonLabelledStatement 'nonLabelledStatement' {{SingleStatement}}
    ;








nonLabelledStatement
    :    block 'block' {{NonLabelledStatement}}
    |    localVariableDeclaration 'localVariableDeclaration' {{NonLabelledStatement}}
    |    forStatement 'forStatement' {{NonLabelledStatement}}
    |    whileStatement 'whileStatement' {{NonLabelledStatement}}
    |    doStatement 'doStatement' {{NonLabelledStatement}}
    |    switchStatement 'switchStatement' {{NonLabelledStatement}}
    |    ifStatement 'ifStatement' {{NonLabelledStatement}}
    |    rethrowStatement 'rethrowStatement' {{NonLabelledStatement}}
    |    tryStatement 'tryStatement' {{NonLabelledStatement}}
    |    breakStatement 'breakStatement' {{NonLabelledStatement}}
    |    continueStatement 'continueStatement' {{NonLabelledStatement}}
    |    returnStatement 'returnStatement' {{NonLabelledStatement}}
    |    localFunctionDeclaration 'localFunctionDeclaration' {{NonLabelledStatement}}
    |    assertStatement 'assertStatement' {{NonLabelledStatement}}
    |    yieldStatement 'yieldStatement' {{NonLabelledStatement}}
    |    yieldEachStatement 'yieldEachStatement' {{NonLabelledStatement}}
    |    expressionStatement 'expressionStatement' {{NonLabelledStatement}}
    ;

expressionStatement
    :    expression 'expressionStmt'? ";" {{ExpressionStatement}}
    ;





localVariableDeclaration
    :    metadata 'metadata' initializedVariableDeclaration 'initializedVariableDeclaration' ";" {{LocalVariableDeclaration}}
    ;

initializedVariableDeclaration
    :    declaredIdentifier 'declaredIdentifier' ("=" expression 'initializeExpression')? ("," initializedIdentifier 'initializedIdentifier')* {{}}
    ;

localFunctionDeclaration
    :    metadata functionSignature functionBody
    ;

ifStatement
    :    <if> "(" expression 'expression' ")" statement 'thenStatement' (<else> statement 'elseStatement')? {{IfStatement}}
    ;

forStatement
    :    <await>? <for> "(" forLoopParts ")" statement 'statement' {{ForStatement}}
    ;

forLoopParts
    :    metadata declaredIdentifier <in> expression
    |    metadata identifier <in> expression
    |    forInitializerStatement expression? ";" expressionList?
    ;



forInitializerStatement
    :    localVariableDeclaration
    |    expression? ";"
    ;

whileStatement
    :    <while> "(" expression 'whileConditionExpression' ")" statement 'whileStatement' {{WhileStatement}}
    ;

doStatement
    :    <do> statement 'doStatement' <while> "(" expression 'doConditionExpression' ")" ";" {{DoStatement}}
    ;

switchStatement
    :    <switch> "(" expression 'switchCaseExpression' ")" <lbrace> switchCase 'cases'* defaultCase 'defaultCase'? <rbrace> {{SwitchStatement}}
    ;

switchCase
    :    label* <case> expression ":" statements
    ;

defaultCase
    :    label* <default> ":" statements
    ;

rethrowStatement
    :    <rethrow> ";"
    ;

tryStatement
    :    <try> block 'block' (onParts finallyPart? | finallyPart) {{TryStatement}}
    ;

onPart
    :    catchPart block
    |    <on> typeNotVoid catchPart? block
    ;

onParts
    :    onPart onParts
    |    onPart
    ;

catchPart
    :    <catch> "(" identifier ("," identifier)? ")"
    ;

finallyPart
    :    <finally> block
    ;

returnStatement
    :    <return> expression 'returnExpression'? ";" {{ReturnStatement}}
    ;

label
    :    identifier 'LabelIdentifier' ":" {{Label}}
    ;

breakStatement
    :    <break> identifier? ";" {{BreakStatement}}
    ;

continueStatement
    :    <continue> identifier? ";" {{ContinueStatement}}
    ;

yieldStatement
    :    <yield> expression 'yieldExpression' ";" {{YieldStatement}}
    ;

yieldEachStatement
    :    <yield> "*" expression 'yieldEachExpression' ";" {{YieldEachStatement}}
    ;

assertStatement
    :    assertion 'assertion' ";" {{AssertStatement}}
    ;

assertion
    :    <ASSERT> "(" expression ("," expression)? ","? ")"
    ;

libraryName
    :    metadata <library> dottedIdentifierList ";"
    ;

dottedIdentifierList
    :    identifier ("." identifier)*
    ;

importOrExport
    :    libraryImport 'libraryImport' 
    |    libraryExport 'libraryExport'
    ;

libraryImport
    :    metadata 'metadata' importSpecification 'importSpecification' {{}}
    ;

/* import 'package:bl_microapp/bl_microapp.dart';*/
importSpecification
    :    <import> configurableUri 'configurableUri' (<deferred>? <as> identifier 'identifier')? combinator* ";" {{}}
    ;

combinator
    :    <show> identifierList
    |    <hide> identifierList
    ;

identifierList
    :    identifier ("," identifier)*
    ;

libraryExport
    :    metadata <export> uri combinator* ";"  {{ExportDeclaration}}
    ;

partDirective
    :    metadata <part> uri ";"
    ;

partHeader
    :    metadata <part> <of> (dottedIdentifierList | uri)";"
    ;

partDeclaration
    :    partHeader topLevelDefinition* 
    ;



uri
    :    stringLiteralWithoutInterpolation 'stringLiteralWithoutInterpolation' {{}}
    ;

configurableUri
    :    uri 'uri' configurationUri* {{}}
    ;

configurationUri
    :    <if> "(" uriTest 'uriTest' ")" uri 'uriConfigured' {{}}
    ;

uriTest
    :    dottedIdentifierList ("==" stringLiteral)? {{}}
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
    |    <void>
    ;

typeNotVoidNotFunction
    :    typeName typeArguments? "?"?
    |    <function> "?"?
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
    :    <typedef> typeIdentifier typeParameters? "=" type ";"
    |    <typedef> functionTypeAlias
    ;

functionTypeAlias
    :    functionPrefix formalParameterPart ";"
    ;

functionPrefix
    :    type identifier
    |    identifier
    ;

functionTypeTail
    :    <function> typeParameters? parameterTypeList
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
    :    <lbrace> namedParameterType ("," namedParameterType)* ","? <rbrace>
    ;

namedParameterType
    :    metadata <required>? typedIdentifier
    ;

typedIdentifier
    :    type identifier
    ;

constructorDesignation
    :    typeIdentifier
    |    qualifiedName
    |    typeName typeArguments ("." (identifier | <new>))?
    ;

symbolLiteral
    :    "#" (operator | (identifier ("." identifier)*) | <void>)
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
    |    <break>
    |    <case>
    |    <catch>
    |    <clazz>
    |    <const>
    |    <continue>
    |    <default>
    |    <do>
    |    <else>
    |    <enum>
    |    <extends>
    |    <false>
    |    <final>
    |    <finally>
    |    <for>
    |    <if>
    |    <in>
    |    <is>
    |    <new>
    |    <null>
    |    <rethrow>
    |    <return>
    |    <super>
    |    <switch>
    |    <this>
    |    <throw>
    |    <true>
    |    <try>
    |    <var>
    |    <void>
    |    <while>
    |    <with>
    ;

builtInIdentifier
    :    <abstract>
    |    <as>
    |    <covariant>
    |    <deferred>
    |    <dynamic>
    |    <export>
    |    <extension>
    |    <external>
    |    <factory>
    |    <function>
    |    <get>
    |    <implements>
    |    <import>
    |    <interface>
    |    <late>
    |    <library>
    |    <operator>
    |    <mixin>
    |    <part>
    |    <required>
    |    <set>
    |    <static>
    |    <typedef>
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

<break>
    :    break
    ;

<case>
    :    case
    ;

<catch>
    :    catch
    ;

<clazz>
    :    class
    ;

<const>
    :    const
    ;

<continue>
    :    continue
    ;

<default>
    :    default
    ;

<do>
    :    do
    ;

<else>
    :    else
    ;

<enum>
    :    enum
    ;

<extends>
    :    extends
    ;

<false>
    :    false
    ;

<final>
    :    final
    ;

<finally>
    :    finally
    ;

<for>
    :    for
    ;

<if>
    :    if
    ;

<in>
    :    in
    ;

<is>
    :    is
    ;

<new>
    :    new
    ;

<null>
    :    null
    ;

<rethrow>
    :    rethrow
    ;

<return>
    :    return
    ;

<super>
    :    super
    ;

<switch>
    :    switch
    ;

<this>
    :    this
    ;

<throw>
    :    throw
    ;

<true>
    :    true
    ;

<try>
    :    try
    ;

<var>
    :    var
    ;

<void>
    :    void
    ;

<while>
    :    while
    ;

<with>
    :    with
    ;

<abstract>
    :    abstract
    ;

<as>
    :    as
    ;

<covariant>
    :    covariant
    ;

<deferred>
    :    deferred
    ;

<dynamic>
    :    dynamic
    ;

<export>
    :    export
    ;

<extension>
    :    extension
    ;

<external>
    :    external
    ;

<factory>
    :    factory
    ;

<function>
    :    Function
    ;

<get>
    :    get
    ;

<implements>
    :    implements
    ;

<import>
    :    import
    ;

<interface>
    :    interface
    ;

<late>
    :    late
    ;

<library>
    :    library
    ;

<operator>
    :    operator
    ;

<mixin>
    :    mixin
    ;

<part>
    :    part
    ;

<required>
    :    required
    ;

<set>
    :    set
    ;

<static>
    :    static
    ;

<typedef>
    :    typedef
    ;



<await>
    :    await
    ;

<yield>
    :    yield
    ;



<async>
    :    async
    ;

<hide>
    :    hide
    ;

<of>
    :    of
    ;

<on>
    :    on
    ;

<show>
    :    show
    ;

<sync>
    :    sync
    ;



<number>
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

<lbrace>
    :    \{ /* TODO: { enterBrace(); }*/
    ;

<rbrace>
    :    /* TODO: { currentBraceLevel(BRACE_NORMAL) } */ /* TODO: { exitBrace(); }*/ \}
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


<NEWLINE>
    :    (\r | \n | \r\n)
    ;

<FEFF>
    :    \xFEFF
    ;

<whitespace>
	: \s+
	;

<comment>
	: \/\/ [^\r\n]* 
	| /\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/
 	;

