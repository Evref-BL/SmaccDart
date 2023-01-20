/* Configuration */

%glr;
%prefix Dart ;
%suffix Node ;
%root Program ;

/*
%nonassoc <MULTI_LINE_STRING_DQ_BEGIN_END> <SINGLE_LINE_STRING_DQ_BEGIN_END>;
*/

/* Hierarchy */
/* Use to create the class heritage hierarchy inside Pharo 
(e.g. ImportDeclaration will be a subClass of ImportExportDeclaration)

%annotate_tokens ;




%hierarchy LibraryDefinition (ImportDeclaration)
%hierarchy Literal (NullLiteral NumericLiteral BooleanLiteral StringLiteral SetOrMapLiteral ListLiteral);
%hierarchy NonLabelledStatement (Block LocalVariableDeclaration ForStatement WhileStatement DoStatement
								SwitchStatement IfStatement RethrowStatement TryStatement BreakStatement
								ContinueStatement ReturnStatement LocalFunctionDeclaration AssertStatement
								YieldStatement YieldEachStatement ExpressionStatement);

%hierarchy Expression (ConditionalExpression AssignableExpressionWithOperator
					Casade ThrowExpression AssignableExpression InitializerExpression
					FunctionExpression ThisExpression NewExpression ConstObjectExpression
					UnaryExpression AdditiveExpression MultiplicativeExpression ShiftExpression
					AwaitExpression PostfixExpression AssignableExpression);

%hierarchy ConditionalExpression (IfNullExpression LogicalOrExpression LogicalAndExpression 													EqualityExpression RelationalExpression BitwiseOrExpression
									BitwiseXorExpression BitwiseAndExpression );

  */
/*grammar Dart;*/



libraryDefinition
    :    <FEFF>? <SCRIPT_TAG>?
         libraryName?
         importOrExport*
         partDirective*
         partDeclaration*
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
    :    <covariant>? finalConstVarOrType 'finalConstVarOrType' identifier 'declaredIdentifier' {{DeclaredIdentifier}}
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
    :    identifier 'identifier' ("=" expression 'expression')? {{InitializedIdentifier}}
    ;

initializedIdentifierList
    :    initializedIdentifier 'initializedIdentifier' ("," initializedIdentifier 'nextInitializedIdentifier')* {{InitializedIdentifierList}}
    ;

functionSignature
    :    type 'type'? identifierNotFUNCTION 'identifierNotFUNCTION' formalParameterPart 'formalParameterPart' {{FunctionSignature}}
    ;

functionBodyPrefix
    :    <async>? "=>"
    |    (<async> | <async> "*" | <sync> "*")? <lbrace>
    ;

functionBody
    :    "=>"  expression 'expression' ";" {{FunctionBody}}
    |     block 'block'  {{FunctionBody}}
    |    <async> "=>" expression 'expression' ";" {{AsyncFunctionBody}}
    |    (<async> | <async> "*" | <sync> "*") block 'block' {{AsyncFunctionBody}}
    ;

block
    :    <lbrace> statements 'statements'? <rbrace> {{Block}}
    ;

formalParameterPart
    :    typeParameters 'typeParameters'? formalParameterList 'formalParameterList' {{FormalParameterPart}}
    ;

formalParameterList
    :    "(" ")"
    |    "(" normalFormalParameters ","? ")"
    |    "(" normalFormalParameters "," optionalOrNamedFormalParameters ")"
    |    "(" optionalOrNamedFormalParameters ")"
    ;

normalFormalParameters
    :    normalFormalParameter 'normalFormalParameter' ("," normalFormalParameter 'nextNormalFormalParameter')* {{NormalFormalParameters}}
    ;

optionalOrNamedFormalParameters
    :    optionalPositionalFormalParameters
    |    namedFormalParameters
    ;

optionalPositionalFormalParameters
    :    "[" defaultFormalParameter 'defaultFormalParameter' ("," defaultFormalParameter 'defaultFormalParameter')* ","? "]" {{OptionalPositionalFormalParameters}}
    ;

namedFormalParameters
    :    <lbrace> defaultNamedParameter 'defaultNamedParameter' ("," defaultNamedParameter 'nextDefaultNamedParameter')* ","? <rbrace> {{NamedFormalParameters}}
    ;

normalFormalParameter
    :    metadata 'metadata' normalFormalParameterNoMetadata 'normalFormalParameterNoMetadata' {{NormalFormalParameter}}
    ;

normalFormalParameterNoMetadata
    :    functionFormalParameter
    |    fieldFormalParameter
    |    simpleFormalParameter
    ;


functionFormalParameter
    :    <covariant>? type 'Type'? identifierNotFUNCTION 'identifierNotFUNCTION' formalParameterPart 'formalParameterPart' "?"? {{FunctionFormalParameter}}
    ;

simpleFormalParameter
    :    declaredIdentifier
    |    <covariant>? identifier 'identifier'
    ;


fieldFormalParameter
    :    finalConstVarOrType 'finalConstVarOrType'? <this> "." identifier 'identifier' (formalParameterPart 'formalParameterPart' "?"?)? {{FieldFormalParameter}}
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
    :   <abstract>? <clazz> typeWithParameters 'typeWithParameters' superclass 'superclass'? mixins 'mixins'? interfaces 'interfaces'? 
        <lbrace> (metadata 'metadata' classMemberDefinition 'classMemberDefinition')* <rbrace> {{ClassDeclaration}}
    |    <abstract>? <clazz> mixinApplicationClass 'mixinApplicationClass' {{ClassDeclaration}}
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
    :    typeWithParameters 'typeWithParameters' "=" mixinApplication 'mixinApplication' ";" {{MixinApplicationClass}}
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
    :    constructorSignature 'constructorSignature' initializers 'initializers' {{MethodSignature}}
    |    factoryConstructorSignature
    |    <static>? functionSignature 'functionSignature' {{MethodSignature}}
    |    <static>? getterSignature 'getterSignature' {{MethodSignature}}
    |    <static>? setterSignature 'setterSignature' {{MethodSignature}}
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
    |    <external> (<static>? finalVarOrType 'finalVarOrType' | <covariant> varOrType 'varOrType') identifierList 'identifierList' {{Declaration}}
    |    <abstract> (finalVarOrType | <covariant> varOrType) identifierList {{Declaration}}
    |    <external>? operatorSignature
    |    <static> (<final> | <const>) type? staticFinalDeclarationList 'staticFinalDeclarationList' {{Declaration}}
    |    <static> <late> <final> type? initializedIdentifierList 'initializedIdentifierList' {{Declaration}}
    |    <static> <late>? varOrType 'varOrType' initializedIdentifierList 'initializedIdentifierList' {{Declaration}}
    |    <covariant> <late> <final> type 'type'? identifierList 'identifierList' {{Declaration}}
    |    <covariant> <late>? varOrType 'varOrType' initializedIdentifierList 'initializedIdentifierList' {{Declaration}}
    |    <late>? (<final> 'finalToken' type 'type'? | varOrType 'varOrType') initializedIdentifierList 'initializedIdentifierList' {{Declaration}}
    |    redirectingFactoryConstructorSignature
    |    constantConstructorSignature 'constantConstructorSignature' (redirection 'redirection' | initializers 'initializers')? {{Declaration}}
    |    constructorSignature 'constructorSignature' (redirection 'redirection' | initializers 'initializers')? {{Declaration}}
    ;

staticFinalDeclarationList
    :    staticFinalDeclaration 'staticFinalDeclaration' ("," staticFinalDeclaration 'nextStaticFinalDeclaration')* {{StaticFinalDeclarationList}}
    ;

staticFinalDeclaration
    :    identifier 'identifier' "=" expression 'expression' {{StaticFinalDeclaration}}
    ;

operatorSignature
    :    type 'type'? <operator> operator 'operator' formalParameterList 'formalParameterList' {{OperatorSignature}}
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
    :    type 'type'? <get> identifier 'identifier' {{GetterSignature}}
    ;

setterSignature
    :    type 'type'? <set> identifier 'identifier' formalParameterList 'formalParameterList' {{SetterSignature}}
    ;

constructorSignature
    :    constructorName 'constructorName' formalParameterList 'formalParameterList' {{ConstructorSignature}}
    ;

constructorName
    :    typeIdentifier 'typeIdentifier' ("." (identifier 'typeIdentifier' | <new>))? {{ConstructorName}}
    ;

redirection
    :    ":" <this> ("." (identifier 'identifier' | <new>))? arguments 'arguments' {{Redirection}}
    ;

initializers
    :    ":" initializerListEntry 'initializerListEntry' ("," initializerListEntry 'InitializerListEntry')* {{Initializers}}
    ;

initializerListEntry
    :    <super> arguments
    |    <super> "." (identifier | <new>) arguments
    |    fieldInitializer
    |    assertion
    ;

fieldInitializer
    :    (<this> ".")? identifier 'identifier' "=" initializerExpression 'initializerExpression' {{FieldInitializer}}
    ;

initializerExpression
    :    conditionalExpression 'conditionalExpression' {{InitializerExpression}}
    |    cascade 'cascade' {{InitializerExpression}}
    ;

factoryConstructorSignature
    :    <const>? <factory> constructorName 'constructorName' formalParameterList 'formalParameterList' {{FactoryConstructorSignature}}
    ;

redirectingFactoryConstructorSignature
    :    <const>? <factory> constructorName 'constructorName' formalParameterList 'formalParameterList' "=" 
         constructorDesignation {{RedirectingFactoryConstructorSignature}}
    ;

constantConstructorSignature
    :    <const> constructorName 'constructorName' formalParameterList 'formalParameterList' {{ConstantConstructorSignature}}
    ;

mixinApplication
    :    typeNotVoidNotFunction 'typeNotVoidNotFunction' mixins 'mixins' interfaces 'interfaces'? {{MixinApplication}}
    ;

enumType
    :    <enum> typeIdentifier typeParameters? mixins? interfaces? <lbrace>
         enumEntry ("," enumEntry)* (",")?
         (";" (metadata classMemberDefinition)*)?
         <rbrace>
    ;

enumEntry
    :    metadata 'metadata' identifier 'identifier' argumentPart 'argumentPart'? {{EnumEntry}}
    |    metadata 'metadata' identifier 'identifier' typeArguments 'typeArguments'? "." identifier 'identifier' arguments 'arguments' {{EnumEntry}}
    ;

typeParameter
    :    metadata 'metadata' typeIdentifier 'typeIdentifier' (<extends> typeNotVoid 'typeNotVoid')? {{TypeParameter}}
    ;

typeParameters
    :    "<" typeParameter 'typeParameter' ("," typeParameter 'nextTypeParameter')* ">" {{TypeParameters}}
    ;

metadata
    :    ("@" metadatum)*
    ;

metadatum
    :    constructorDesignation 'constructorDesignation' arguments 'arguments' {{Metadatum}}
    |    identifier
    |    qualifiedName
    ;


expression 
    :    assignableExpression 'assignableExpression' assignmentOperator 'assignmentOperator' expression 'assignedExpression' 
    |    conditionalExpression 'conditionalExpression' 
    |    cascade 'cascade'
    |    throwExpression 'throwExpression' 
    ;

/*
expression 
    :    assignableExpression 'assignableExpression' assignmentOperator 'assignmentOperator' expression 'assignedExpression' {{Expression}}
    |    conditionalExpression 'conditionalExpression' {{Expression}}
    |    cascade 'cascade' {{Expression}}
    |    throwExpression 'throwExpression' {{Expression}}
    ;





expression
    :   assignableExpressionWithOperator 'assignableExpressionWithOperator' {{Expression}}
    |   cascade 'cascade' {{Expression}}
    |   primary 'primary' {{Expression}}
    |   unaryExpression 'unaryExpression' {{Expression}}
    |   initializerExpression 'initializerExpression' {{Expression}}
    |   ifNullExpression 'ifNullExpression' {{Expression}}
    |   logicalOrExpression 'logicalOrExpression' {{Expression}}
    |   logicalAndExpression  'logicalAndExpression' {{Expression}}
    |   equalityExpression 'equalityExpression' {{Expression}}
    |   relationalExpression 'relationalExpression' {{Expression}}
    |   bitwiseOrExpression 'bitwiseOrExpression' {{Expression}}
    |   bitwiseXorExpression 'bitwiseXorExpression' {{Expression}}
    |   bitwiseAndExpression 'bitwiseAndExpression' {{Expression}}
    |   shiftExpression 'shiftExpression' {{Expression}}
    |   additiveExpression 'additiveExpression' {{Expression}}
    |   multiplicativeExpression 'multiplicativeExpression' {{Expression}}
    |   throwExpression 'throwExpression' {{Expression}}
    |   functionExpression 'functionExpression' {{Expression}}
    |   thisExpression 'thisExpression' {{Expression}}
    |   newExpression 'newExpression' {{Expression}}
    |   constObjectExpression 'constObjectExpression' {{Expression}}
    |   awaitExpression 'awaitExpression' {{Expression}}
    |   postfixExpression 'postfixExpression' {{Expression}}
    ;

 */



assignableExpressionWithOperator
    :   assignableExpression 'assignableExpression' assignmentOperator 'assignmentOperator' expression 'assigmnentExpression' {{AssignableExpressionWithOperator}}
    ;

expressionWithoutCascade
    :    assignableExpressionWithoutCascadeWithOperator 
    |    functionExpressionWithoutCascade
    |    conditionalExpression
    |    throwExpressionWithoutCascade
    ;

assignableExpressionWithoutCascadeWithOperator
    :   assignableExpression 'assignableExpression' assignmentOperator 'assignmentOperator' expressionWithoutCascade 'expressionWithoutCascade' {{AssignableExpressionWithoutCascadeWithOperator}}
    ;

expressionList
    :    expression 'firstExpression' ("," expression 'nextExpression')* {{ExpressionList}}
    ;

/*
primary
    :    thisExpression 'thisExpression' {{Primary}}
    |    <super> unconditionalAssignableSelector 'unconditionalAssignableSelector' {{Primary}}
    |    <super> argumentPart 'argumentPart' {{Primary}}
    |    functionExpression 'functionExpression' {{Primary}}
    |    literal 'literal' {{Primary}}
    |    identifier 'identifier' {{Primary}}
    |    newExpression 'newExpression' {{Primary}}
    |    constObjectExpression 'constObjectExpression' {{Primary}}
    |    constructorInvocation 'constructorInvocation' {{Primary}}
    |    functionPrimary 'functionPrimary' {{Primary}}
    |    "(" expression 'parenthesisExpression' ")" {{Primary}}
    |    constructorTearoff 'constructorTearoff' {{Primary}}
    ;
 */

primary
    :    thisExpression 
    |    <super> unconditionalAssignableSelector 'unconditionalAssignableSelector' {{Primary}}
    |    <super> argumentPart 'argumentPart' {{Primary}}
    |    functionExpression 
    |    literal 
    |    identifier
    |    newExpression 
    |    constObjectExpression 
    |    constructorInvocation
    |    functionPrimary 
    |    "(" expression 'parenthesisExpression' ")" {{Primary}}
    |    constructorTearoff
    ;

constructorInvocation
    :    typeName 'typeName' typeArguments 'typeArguments' "." <new> arguments 'arguments' {{ConstructorInvocation}}
    |    typeName 'typeName' "." <new> arguments 'arguments' {{ConstructorInvocation}}
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
    :    <null> {{NullLiteral}}
    ;

numericLiteral
    :    <number> 'number' {{NumericLiteral}}
    |    <HEX_NUMBER> 'hexNumber'  {{NumericLiteral}}
    ;

booleanLiteral
    :    <true> 'true' {{BooleanLiteral}}
    |    <false> 'false' {{BooleanLiteral}}
    ;


 
stringLiteral
    :    (multiLineString 'multiLineString' | singleLineString 'singleLineString')+ {{StringLiteral}}
    ;



stringLiteralWithoutInterpolation
    :    singleStringWithoutInterpolation+
    ;

setOrMapLiteral
    : <const>? typeArguments 'typeArguments'? <lbrace> elements 'elements'? <rbrace> {{SetOrMapLiteral}}
    ;

listLiteral
    : <const>? typeArguments 'typeArguments'? "[" elements 'elements'? "]" {{ListLiteral}}
    ;

elements
    : element 'element' ("," element 'nextElements')* ","? {{Elements}}
    ;

element
    : expressionElement 'expressionElement' {{Element}}
    | mapElement 'mapElement' {{Element}}
    | spreadElement 'spreadElement' {{Element}}
    | ifElement 'ifElement' {{Element}}
    | forElement 'forElement' {{Element}}
    ;

expressionElement
    : expression 'expression' {{ExpressionElement}}
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
    :    <throw> expression 'throwExpression' {{ThrowExpression}}
    ;

throwExpressionWithoutCascade
    :    <throw> expressionWithoutCascade
    ;

functionExpression
    :    formalParameterPart 'formalParameterPart' functionExpressionBody 'functionExpressionBody' {{FunctionExpression}}
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
    :    <this> {{ThisExpression}}
    ;

newExpression
    :    <new>? constructorDesignation 'constructorDesignation' arguments 'constructorArguments' {{NewExpression}}
    ;

constObjectExpression
    :    <const> constructorDesignation 'constructorDesignation' arguments 'arguments' {{ConstObjectExpression}}
    ;

arguments
    :    "(" (argumentList ","?)? ")"
    ;

argumentList
    :    namedArgument 'namedArgument' ("," namedArgument 'namedArgument')* {{ArgumentList}}
    |    expressionList 'expressionList' ("," namedArgument 'namedArgument')* {{ArgumentList}}
    ;

namedArgument
    :    label 'label' expression 'expression' {{NamedArgument}}
    ;

cascade
    :     cascade 'cascade' ".." cascadeSection 'cascadeSection' {{Cascade}}
    |     conditionalExpression 'conditionalExpression' ("?.." | "..") cascadeSection 'cascadeSection' {{Cascade}}
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
    :    "=" 'equalsSymbol' {{AssignmentOperator}}
    |    compoundAssignmentOperator 'compoundAssignmentOperator' {{AssignmentOperator}}
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

/* original 
conditionalExpression
    :    ifNullExpression 'ifNullExpression' 
    ( "?" expressionWithoutCascade 'firstExpressionWithoutCascade' ":" 
    expressionWithoutCascade 'secondExpressionWithoutCascade' )? {{ConditionalExpression}}
    ;
*/

conditionalExpression
    :   ifNullExpression   
    |   ifNullExpression 'testExpression' 
    "?" expressionWithoutCascade 'trueExpression' ":" 
    expressionWithoutCascade 'falseExpression' {{ConditionalExpression}}
    ;

/* original
ifNullExpression
    : logicalOrExpression 'testNullExpression' ("??" logicalOrExpression 'logicalOrExpression')* {{IfNullExpression}}
    ;
 */


ifNullExpression
    :   logicalOrExpression
    |   logicalOrExpression 'testNullExpression' ("??" logicalOrExpression 'logicalOrExpression')+ {{IfNullExpression}}
    ;


/* original
logicalOrExpression
    :    logicalAndExpression 'logicalAndExpression' ("||" logicalAndExpression 'logicalAndExpression')* {{LogicalOrExpression}}
    ;
 */

logicalOrExpression
    :   logicalAndExpression
    |   logicalAndExpression 'logicalAndExpression' ("||" logicalAndExpression 'logicalAndExpression')+ {{BinaryExpression}}
    ;

/* original
logicalAndExpression
    :    equalityExpression 'equalityExpression' ("&&" equalityExpression 'equalityExpression')* {{LogicalAndExpression}}
    ;
 */
logicalAndExpression
    :   equalityExpression
    |   equalityExpression 'equalityExpression' ("&&" equalityExpression 'equalityExpression')+ {{BinaryExpression}}
    ;

/* original
equalityExpression
    :   relationalExpression 'relationalExpression' (equalityOperator 'equalityOperator' relationalExpression 'relationalExpression')? {{EqualityExpression}}
    |   <super> equalityOperator 'equalityOperator' relationalExpression 'relationalExpression' {{EqualityExpression}}
    ;
*/

equalityExpression
    :   relationalExpression 
    |   relationalExpression 'relationalExpression' (equalityOperator 'equalityOperator' relationalExpression 'relationalExpression')+ {{BinaryExpression}}
    |   <super> equalityOperator 'equalityOperator' relationalExpression 'relationalExpression' {{BinaryExpression}}
    ;

equalityOperator
    :    "=="
    |    "!="
    ;

/*
relationalExpression
    :   bitwiseOrExpression 'bitwiseOrExpression'
        (typeTest 'typeTest' | typeCast 'typeCast' | relationalOperator 'relationalOperator' bitwiseOrExpression 'bitwiseOrExpression' )? {{RelationalExpression}}
    |    <super> relationalOperator 'relationalOperator' bitwiseOrExpression 'bitwiseOrExpression' {{RelationalExpression}}
    ;
 */

relationalExpression
    :   bitwiseOrExpression
    |   bitwiseOrExpression 'bitwiseOrExpression'
        (typeTest 'typeTest' | typeCast 'typeCast' | relationalOperator 'relationalOperator' bitwiseOrExpression 'bitwiseOrExpression')+ {{BinaryExpression}}
    |    <super> relationalOperator 'relationalOperator' bitwiseOrExpression 'bitwiseOrExpression' {{BinaryExpression}}
    ;

relationalOperator
    :    ">" "="
    |    ">"
    |    "<="
    |    "<"
    ;

/* original 
bitwiseOrExpression
    :    bitwiseXorExpression 'bitwiseXorExpression' ("|" bitwiseXorExpression 'bitwiseXorExpression')* {{BitwiseOrExpression}}
    |    <super> ("|" bitwiseXorExpression 'bitwiseXorExpression')+ {{BitwiseOrExpression}}
    ;
*/

bitwiseOrExpression
    :   bitwiseXorExpression
    |   bitwiseXorExpression 'bitwiseXorExpression' ("|" bitwiseXorExpression 'bitwiseXorExpression')+ {{BinaryExpression}}
    |   <super> "|" bitwiseXorExpression 'bitwiseXorExpression' {{BinaryExpression}}
    ;

/* original
bitwiseXorExpression
    :    bitwiseAndExpression 'bitwiseAndExpression' ("^" bitwiseAndExpression 'bitwiseAndExpression')* {{BitwiseXorExpression}}
    |    <super> ("^" bitwiseAndExpression 'bitwiseAndExpression')+ {{BitwiseXorExpression}}
    ;
 */
bitwiseXorExpression
    :   bitwiseAndExpression
    |   bitwiseAndExpression 'bitwiseAndExpression'( "^" bitwiseAndExpression 'bitwiseAndExpression')+ {{BinaryExpression}}
    |   <super> "^" bitwiseAndExpression 'bitwiseAndExpression' {{BinaryExpression}}
    ;

/* original 
bitwiseAndExpression
    :    shiftExpression 'shiftExpression' ("&" shiftExpression 'shiftExpression')* {{BitwiseAndExpression}}
    |    <super> ("&" shiftExpression 'shiftExpression')+ {{BitwiseAndExpression}}
    ;
 */

bitwiseAndExpression
    :   shiftExpression
    |   shiftExpression 'shiftExpression' ("&" shiftExpression 'shiftExpression')+ {{BinaryExpression}}
    |   <super> "&" shiftExpression 'shiftExpression' {{BinaryExpression}}
    ;

bitwiseOperator
    :    "&"
    |    "^"
    |    "|"
    ;

/* original
shiftExpression
    :    additiveExpression 'additiveExpression' (shiftOperator 'shiftOperator' additiveExpression 'additiveExpression')* {{ShiftExpression}}
    |    <super> (shiftOperator 'shiftOperator' additiveExpression 'additiveExpression')+ {{ShiftExpression}}
    ;
 */

shiftExpression
    :   additiveExpression 
    |   additiveExpression 'additiveExpression'( shiftOperator 'shiftOperator' additiveExpression 'additiveExpression')+ {{BinaryExpression}}
    |   <super> shiftOperator 'shiftOperator' additiveExpression 'additiveExpression' {{BinaryExpression}}
    ;

shiftOperator
    :    "<<"
    |    ">" ">" ">"
    |    ">" ">"
    ;

/* original
additiveExpression
    :    multiplicativeExpression 'multiplicativeExpression' (additiveOperator 'additiveOperator' multiplicativeExpression 'multiplicativeExpression')* {{AdditiveExpression}}
    |    <super> (additiveOperator 'additiveOperator' multiplicativeExpression 'multiplicativeExpression')+ {{AdditiveExpression}}
    ;
 */

additiveExpression
    :   multiplicativeExpression
    |   multiplicativeExpression 'multiplicativeExpression' (additiveOperator 'additiveOperator' multiplicativeExpression 'multiplicativeExpression')+ {{AdditiveExpression}}
    |   <super> additiveOperator 'additiveOperator' multiplicativeExpression 'multiplicativeExpression' {{AdditiveExpression}}
    ;

additiveOperator
    :    "+"
    |    "-"
    ;

/*
multiplicativeExpression
    :    unaryExpression 'unaryExpression' (multiplicativeOperator 'multiplicativeOperator' unaryExpression 'unaryExpression')* {{MultiplicativeExpression}}
    |    <super> (multiplicativeOperator 'multiplicativeOperator' unaryExpression 'unaryExpression')+ {{MultiplicativeExpression}}
    ;
 */

multiplicativeExpression
    :   unaryExpression
    |   unaryExpression 'unaryExpression' (multiplicativeOperator 'multiplicativeOperator' unaryExpression 'unaryExpression')+ {{MultiplicativeExpression}}
    |   <super> multiplicativeOperator 'multiplicativeOperator' unaryExpression 'unaryExpression' {{MultiplicativeExpression}}
    ;

multiplicativeOperator
    :    "*"
    |    "/"
    |    "%"
    |    "~/"
    ;

/*
unaryExpression
    :    prefixOperator 'prefixOperator' unaryExpression 'unaryExpression' {{UnaryExpression}}
    |    awaitExpression 'awaitExpression' {{UnaryExpression}}
    |    postfixExpression 'postfixExpression' {{UnaryExpression}}
    |    (minusOperator 'minusOperator' | tildeOperator 'tildeOperator') <super> {{UnaryExpression}}
    |    incrementOperator 'incrementOperator' assignableExpression 'assignableExpression' {{UnaryExpression}}
    ;
*/

unaryExpression
    :    prefixOperator 'prefixOperator' unaryExpression 'unaryExpression' {{UnaryExpression}}
    |    awaitExpression 
    |    postfixExpression 
    |    (minusOperator 'minusOperator' | tildeOperator 'tildeOperator') <super> {{UnaryExpression}}
    |    incrementOperator 'incrementOperator' assignableExpression 'assignableExpression' {{UnaryExpression}}
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
    :    <await> unaryExpression 'unaryExpression' {{AwaitExpression}}
    ;

postfixExpression
    :    assignableExpression 'assignableExpression' postfixOperator 'postfixOperator' {{PostfixExpression}}
    |    primary 'primary' selector 'selector'* {{PostfixExpression}}
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
    :    primary 'primary' assignableSelectorPart 'assignableSelectorPart' {{AssignableExpression}}
    |    <super> unconditionalAssignableSelector 'unconditionalAssignableSelector' {{AssignableExpression}}
    |    identifier 'identifier' {{AssignableExpression}}
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
    |    (<await>|<yield>)
    ;

identifier
    :    identifierNotFUNCTION 'idNotFunction' {{Identifier}}
    |    <function> 'function' {{Identifier}}
    ;

qualifiedName
    :    typeIdentifier 'typeIdentifier' "." (identifier 'identifier' | <new>) {{QualifiedName}}
    |    typeIdentifier 'typeIdentifier' "." typeIdentifier 'typeIdentifier' "." (identifier 'identifier' | <new>) {{QualifiedName}}
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

/*
statements
    :    statement 'statementsSet'* {{SequentialStatements}}
    ;
 */
statements
    : statement 'statement' {{Statements}}
    | statements statement 'statement' {{Statements}}
    ;

statement
    :    label 'label'* nonLabelledStatement 'nonLabelledStatement' {{SingleStatement}}
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

/*
expressionStatement
    :    expression 'expressionStmt'? ";" {{ExpressionStatement}}
    ;
 */

expressionStatement
    :    expression? ";" 
    ;

localVariableDeclaration
    :    metadata 'metadata' initializedVariableDeclaration 'initializedVariableDeclaration' ";" {{LocalVariableDeclaration}}
    ;

initializedVariableDeclaration
    :    declaredIdentifier 'declaredIdentifier' ("=" expression 'initializeExpression')? ("," initializedIdentifier 'initializedIdentifier')* {{InitializedVariableDeclaration}}
    ;

localFunctionDeclaration
    :    metadata 'metadata' functionSignature 'functionSignature' functionBody 'functionBody' {{LocalFunctionDeclaration}}
    ;

ifStatement
    :    <if> "(" expression 'ifConditionalExpression' ")" statements 'ifThenStatement' (<else> statement 'elseStatement')? {{IfStatement}}
    ;

forStatement
    :    <await>? <for> "(" forLoopParts ")" statements 'statement' {{ForStatement}}
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
    :    <while> "(" expression 'whileConditionExpression' ")" statements 'whileStatement' {{WhileStatement}}
    ;

doStatement
    :    <do> statement 'doStatement' <while> "(" expression 'doConditionExpression' ")" ";" {{DoStatement}}
    ;

switchStatement
    :    <switch> "(" expression 'switchCaseExpression' ")" <lbrace> switchCase 'cases'* defaultCase 'defaultCase'? <rbrace> {{SwitchStatement}}
    ;

switchCase
    :    label 'label'* <case> expression 'expression' ":" statements 'statements'? {{SwitchCase}}
    ;

defaultCase
    :    label 'label'* <default> ":" statements 'statements'? {{DefaultCase}}
    ;

rethrowStatement
    :    <rethrow> ";" {{RethrowStatement}}
    ;

tryStatement
    :    <try> block 'block' (onParts 'onParts' finallyPart 'finallyPart'? | finallyPart 'finallyPart') {{TryStatement}}
    ;

onPart
    :    catchPart 'catchPart' block 'block' {{OnPart}}
    |    <on> typeNotVoid 'typeNotVoid' catchPart 'catchPart'? block 'block' {{OnPart}}
    ;

onParts
    :    onPart onParts
    |    onPart
    ;

catchPart
    :    <catch> "(" identifier 'identifier' ("," identifier 'identifier')? ")" {{CatchPart}}
    ;

finallyPart
    :    <finally> block 'block' {{FinallyPart}}
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
    :    <ASSERT> "(" expression 'expression' ("," expression 'expression')? ","? ")" {{Assertion}}
    ;

libraryName
    :    metadata 'metadata' <library> dottedIdentifierList 'dottedIdentifierList' ";" {{LibraryName}}
    ;

dottedIdentifierList
    :    identifier 'identifier' ("." identifier 'identifier')* {{DottedIdentifierList}}
    ;

importOrExport
    :    libraryImport 'libraryImport' 
    |    libraryExport 'libraryExport'
    ;

libraryImport
    :    metadata 'metadata' importSpecification 'importSpecification' {{LibraryImport}}
    ;

/* import 'package:bl_microapp/bl_microapp.dart';*/
importSpecification
    :    <import> configurableUri 'configurableUri' (<deferred>? <as> identifier 'identifier')? combinator* ";" {{ImportSpecification}}
    ;

combinator
    :    <show> identifierList
    |    <hide> identifierList
    ;

identifierList
    :    identifier 'identifier' ("," identifier 'nextIdentifier')* {{IdentifierList}}
    ;

/*
libraryExport
    :    metadata <export> uri combinator* ";"  {{ExportDeclaration}}
    ;
*/

libraryExport
    :    metadata <export> configurableUri 'configurableUri' combinator* ";"  {{ExportDeclaration}}
    ;

partDirective
    :    metadata 'metadata' <part> uri 'uri' ";" {{PartDirective}}
    ;

partHeader
    :    metadata <part> <of> (dottedIdentifierList | uri)";" {{PartHeader}}
    ;

partDeclaration
    :    partHeader topLevelDefinition* {{PartDeclaration}}
    ;



uri
    :    stringLiteralWithoutInterpolation 'stringLiteralWithoutInterpolation' {{Uri}}
    ;

configurableUri
    :    uri 'uri' configurationUri 'configurationUri'* {{ConfigurableUri}}
    ;

configurationUri
    :    <if> "(" uriTest 'uriTest' ")" uri 'uriConfigured' {{ConfigurationUri}}
    ;

uriTest
    :    dottedIdentifierList 'dottedIdentifierList' ("==" stringLiteral 'stringLiteral')? {{UriTest}}
    ;

type
    :    functionType 'functionType' "?"? {{Type}}
    |    typeNotFunction 'typeNotFunction' {{Type}}
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
    :    typeIdentifier 'typeIdentifier' ("." typeIdentifier 'followingTypeIdentifier')? {{TypeName}}
    ;

typeArguments
    :    "<" typeList ">"
    ;

typeList
    :    type 'type' ("," type 'nextType')* {{TypeList}}
    ;

typeNotVoidNotFunctionList
    :    typeNotVoidNotFunction 'typeNotVoidNotFunction' ("," typeNotVoidNotFunction 'nextTypeNotVoidNotFunction')* {{TypeNotVoidNotFunctionList}}
    ;

typeAlias
    :    <typedef> typeIdentifier 'typeIdentifier' typeParameters 'typeParameters'? "=" type 'type' ";" {{TypeAlias}}
    |    <typedef> functionTypeAlias
    ;

functionTypeAlias
    :    functionPrefix 'functionPrefix' formalParameterPart 'formalParameterPart' ";" {{FunctionTypeAlias}}
    ;

functionPrefix
    :    type 'type' identifier 'identifier' {{FunctionPrefix}}
    |    identifier
    ;

functionTypeTail
    :    <function> typeParameters 'typeParameters'? parameterTypeList 'parameterTypeList' {{FunctionTypeTail}}
    ;

functionTypeTails
    :    functionTypeTail 'functionTypeTail' "?"? functionTypeTails 'functionTypeTails' {{FunctionTypeTails}}
    |    functionTypeTail
    ;

functionType
    :    functionTypeTails
    |    typeNotFunction 'typeNotFunction' functionTypeTails 'functionTypeTails' {{FunctionType}}
    ;

parameterTypeList
    :    "(" ")"
    |    "(" normalParameterTypes "," optionalParameterTypes ")"
    |    "(" normalParameterTypes ","? ")"
    |    "(" optionalParameterTypes ")"
    ;

normalParameterTypes
    :    normalParameterType 'normalParameterType' ("," normalParameterType 'nextNormalParameterType')* {{NormalParameterTypes}}
    ;

normalParameterType
    :    metadata 'metadata' typedIdentifier 'typedIdentifier' {{NormalParameterType}}
    |    metadata 'metadata' type 'type' {{NormalParameterType}}
    ;

optionalParameterTypes
    :    optionalPositionalParameterTypes
    |    namedParameterTypes
    ;

optionalPositionalParameterTypes
    :    "[" normalParameterTypes ","? "]"
    ;

namedParameterTypes
    :    <lbrace> namedParameterType 'namedParameterType' ("," namedParameterType 'nextNamedParameterType')* ","? <rbrace> {{NamedParameterTypes}}
    ;

namedParameterType
    :    metadata 'metadata' <required>? typedIdentifier 'typedIdentifier' {{NamedParameterType}}
    ;

typedIdentifier
    :    type 'type' identifier 'identifier' {{TypedIdentifier}}
    ;

constructorDesignation
    :    typeIdentifier
    |    qualifiedName
    |    typeName 'typeName' typeArguments 'typeArguments' ("." (identifier 'identifier' | <new>))? {{ConstructorDesignation}}
    ;

symbolLiteral
    :    "#" (operator 'operator' | (identifier 'identifier' ("." identifier 'identifier')*) | <void>) {{SymbolLiteral}}
    ;


singleStringWithoutInterpolation
    :    <RAW_SINGLE_LINE_STRING>
    |    <RAW_MULTI_LINE_STRING>
    |    <SINGLE_LINE_STRING_DQ_BEGIN_END>
    |    <SINGLE_LINE_STRING_SQ_BEGIN_END>
    |    <MULTI_LINE_STRING_DQ_BEGIN_END> 
    |    <MULTI_LINE_STRING_SQ_BEGIN_END>
    ;

/* SQ : string with single quote
    DQ : double quote
    BEGIN_END : reach the second quote without $ interruption (for string interpolation expresion)
    MID : reach a $*/

/*
multiLineString
    :    <RAW_MULTI_LINE_STRING> 'rawMultiString' {{MultiLineString}}
    |    <MULTI_LINE_STRING_SQ_BEGIN_END> 'multiStringSQBeginEnd' {{MultiLineString}}
    |    <MULTI_LINE_STRING_SQ_BEGIN_MID> 'multiStringSQBeginMid' expression 'firstExpression' (<MULTI_LINE_STRING_MID_MID> expression 'nextExpressions')* <MULTI_LINE_STRING_SQ_MID_END> 'multiStringSQMidEnd' {{MultiLineString}}
    |    <MULTI_LINE_STRING_DQ_BEGIN_END> 'multiStringDQBeginEnd' {{MultiLineString}}
    |    <MULTI_LINE_STRING_DQ_BEGIN_MID> 'multiStringDQBeginMid' expression 'firstExpression' (<MULTI_LINE_STRING_MID_MID> expression 'nextExpressions')* <MULTI_LINE_STRING_DQ_MID_END> 'multiStringDQMidEnd' {{MultiLineString}}
    ;


singleLineString
    :    <RAW_SINGLE_LINE_STRING> 'rawString' {{SingleLineString}}
    |    <SINGLE_LINE_STRING_SQ_BEGIN_END> 'stringSQBeginEnd' {{SingleLineString}}
    |    <SINGLE_LINE_STRING_SQ_BEGIN_MID> 'stringSQBeginMid' expression 'firstExpression' (<SINGLE_LINE_STRING_MID_MID> expression 'nextExpressions')* <SINGLE_LINE_STRING_SQ_MID_END> 'stringSQMidEnd' {{SingleLineString}}
    |    <SINGLE_LINE_STRING_DQ_BEGIN_END> 'stringDQBeginEnd'{{SingleLineString}}
    |    <SINGLE_LINE_STRING_DQ_BEGIN_MID> 'stringDQBeginMid' expression 'firstExpression' (<SINGLE_LINE_STRING_MID_MID> expression 'nextExpressions')* <SINGLE_LINE_STRING_DQ_MID_END> 'stringDQMidEnd' {{SingleLineString}}
    ;
*/

multiLineString
    :    <RAW_MULTI_LINE_STRING> 'rawMultiString' {{MultiLineString}}
    |    <MULTI_LINE_STRING_SQ_BEGIN_END> 'stringBeginEnd' {{MultiLineString}}
    |    <MULTI_LINE_STRING_SQ_BEGIN_MID> 'stringBeginMid' expression 'firstExpression' (<SINGLE_LINE_STRING_MID_MID> 'stringMidMid' expression 'nextExpressions')* <MULTI_LINE_STRING_SQ_MID_END> 'stringMidEnd' {{MultiLineString}}
    |    <MULTI_LINE_STRING_DQ_BEGIN_END> 'stringBeginEnd' {{MultiLineString}}
    |    <MULTI_LINE_STRING_DQ_BEGIN_MID> 'stringBeginMid' expression 'firstExpression' (<SINGLE_LINE_STRING_MID_MID> expression 'nextExpressions')* <MULTI_LINE_STRING_DQ_MID_END> 'stringMidEnd' {{MultiLineString}}
    ;


singleLineString
    :    <RAW_SINGLE_LINE_STRING> 'rawString' {{SingleLineString}}
    |    <SINGLE_LINE_STRING_SQ_BEGIN_END> 'stringBeginEnd' {{SingleLineString}}
    |    <SINGLE_LINE_STRING_SQ_BEGIN_MID> 'stringBeginMid' expression 'firstExpression' (<SINGLE_LINE_STRING_MID_MID> 'stringMidMid' expression 'nextExpressions')* <SINGLE_LINE_STRING_SQ_MID_END> 'stringMidEnd' {{SingleLineString}}
    |    <SINGLE_LINE_STRING_DQ_BEGIN_END> 'stringBeginEnd'{{SingleLineString}}
    |    <SINGLE_LINE_STRING_DQ_BEGIN_MID> 'stringBeginMid' expression 'firstExpression' (<SINGLE_LINE_STRING_MID_MID> 'stringMidMid' expression 'nextExpressions')* <SINGLE_LINE_STRING_DQ_MID_END> 'stringMidEnd' {{SingleLineString}}
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
    :    r <SQ> ([^\'\r\n])* <SQ>
    |    r <DQ> ([^\"\r\n])* <DQ>
    ;

<RAW_MULTI_LINE_STRING>
    :    r <TDQ> (.)* <TDQ>
    |    r <TSQ> (.)* <TSQ>
    ;

<SIMPLE_STRING_INTERPOLATION>
    :    <DOLLAR_IDENTIFIER> <IDENTIFIER_NO_DOLLAR>
    ;

<ESCAPE_SEQUENCE>
    :    \\n
    |    \\r
    |    \\b
    |    \\t
    |    \\v
    |    \\x <HEX_DIGIT> <HEX_DIGIT>
    |    \\u <HEX_DIGIT> <HEX_DIGIT> <HEX_DIGIT> <HEX_DIGIT>
    |    \\u <lbrace> <HEX_DIGIT_SEQUENCE> <rbrace>
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
    |    <DQ> /*case where a string is a single DQ symbol*/
    ;

<SINGLE_LINE_STRING_SQ_BEGIN_END>
    :     <SQ> (<STRING_CONTENT_SQ>)* <SQ>
    ;

<SINGLE_LINE_STRING_SQ_BEGIN_MID>
    :     <SQ> (<STRING_CONTENT_SQ>)* <DOLLAR_IDENTIFIER><lbrace> 
    ;


<SINGLE_LINE_STRING_SQ_MID_MID>
    :   <rbrace> <STRING_CONTENT_SQ> <DOLLAR_IDENTIFIER><lbrace>  
    ;


<SINGLE_LINE_STRING_SQ_MID_END>
    :    <rbrace> (<STRING_CONTENT_SQ>)* <SQ>
    ;

<STRING_CONTENT_DQ>
    :    <STRING_CONTENT_COMMON>
    |    <SQ> /*case where a string is a single SQ symbol*/
    ;

<SINGLE_LINE_STRING_DQ_BEGIN_END>
    :    <DQ> (<STRING_CONTENT_DQ>)* <DQ>
    ;

<SINGLE_LINE_STRING_DQ_BEGIN_MID>
    :    <DQ> (<STRING_CONTENT_DQ>)* <DOLLAR_IDENTIFIER><lbrace> 
    ;


<SINGLE_LINE_STRING_DQ_MID_MID>
    :   <rbrace> (<STRING_CONTENT_DQ>)* <DOLLAR_IDENTIFIER><lbrace>   
    ;


<SINGLE_LINE_STRING_DQ_MID_END>
    :   <rbrace> <STRING_CONTENT_DQ>* <DQ>
    ;

/*new rule*/
<SINGLE_LINE_STRING_MID_MID>
    :   <rbrace> (<STRING_CONTENT_DQ>|<STRING_CONTENT_SQ>)* <DOLLAR_IDENTIFIER><lbrace>   
    ;



<SQ>
    : \'
    ;

<DQ>
    : \"
    ;

<TSQ>
    :   \'\'\'
    ;

<TDQ>
    :   \"\"\"
    ;


/*
<QUOTES_SQ>
    :
    |    <SQ>
    |    <SQ><SQ>
    ;
*/
<QUOTES_SQ>
    :
    |    \'
    |    \'\'
    ;

<ESCAPE_R>
    :   \\\r
    ;

<ESCAPE_N>
    :   \\\n
    ;

<STRING_CONTENT_TSQ>
    :   <QUOTES_SQ> (<STRING_CONTENT_COMMON> | <DQ> |<NEWLINE> | <ESCAPE_R> | <ESCAPE_N>)
    ;

<MULTI_LINE_STRING_SQ_BEGIN_END>
    :   <TSQ> <STRING_CONTENT_TSQ>* <TSQ>
    ;

<MULTI_LINE_STRING_SQ_BEGIN_MID>
    :    <TSQ> <STRING_CONTENT_TSQ>* <QUOTES_SQ> <DOLLAR_IDENTIFIER><lbrace> 
    ;

<MULTI_LINE_STRING_SQ_MID_MID>
    :   <rbrace> <STRING_CONTENT_TSQ>* <QUOTES_SQ> <DOLLAR_IDENTIFIER><lbrace>
         
    ;

<MULTI_LINE_STRING_SQ_MID_END>
    :   <rbrace> <STRING_CONTENT_TSQ>* <TSQ>
    ;


/* 
<QUOTES_DQ>
    :
    |    <DQ>
    |    <DQ><DQ>
    ;
*/

<QUOTES_DQ>
    :
    |    \"
    |    \"\"
    ;


<STRING_CONTENT_TDQ>
    :    <QUOTES_DQ> (<STRING_CONTENT_COMMON> | <SQ> | <NEWLINE> | <ESCAPE_R> | <ESCAPE_N>)
    ;

<MULTI_LINE_STRING_DQ_BEGIN_END>
    :   <TSQ> <STRING_CONTENT_TDQ>* <TSQ>
    ;

<MULTI_LINE_STRING_DQ_BEGIN_MID>
    :   <TSQ> <STRING_CONTENT_TDQ>* <QUOTES_DQ> <DOLLAR_IDENTIFIER><lbrace>
    ;

<MULTI_LINE_STRING_DQ_MID_MID>
    :   <rbrace> <STRING_CONTENT_TDQ>* <QUOTES_DQ> <DOLLAR_IDENTIFIER><lbrace>
    ;

<MULTI_LINE_STRING_DQ_MID_END>
    :   <rbrace> <STRING_CONTENT_TDQ>* <TSQ>
    ;

<MULTI_LINE_STRING_MID_MID>
    :   <rbrace> (<STRING_CONTENT_TSQ>|<STRING_CONTENT_TDQ>)* <QUOTES_DQ> <DOLLAR_IDENTIFIER><lbrace>
    ;

<lbrace>
    :    \{ 
    ;

<rbrace>
    :    \}
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
    |    <DOLLAR_IDENTIFIER>
    ;

<DOLLAR_IDENTIFIER>
    :   \$
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

