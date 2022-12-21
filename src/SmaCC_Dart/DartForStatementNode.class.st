Class {
	#name : #DartForStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'finalConstVarOrType',
		'declaredIdentifier',
		'assignableExpressions',
		'assignmentOperators',
		'assignedExpressions',
		'conditionalExpressions',
		'cascades',
		'throwExpressions',
		'idNotFunction',
		'function',
		'metadata',
		'initializedVariableDeclaration',
		'firstExpression',
		'nextExpressions',
		'statement'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartForStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitForStatement: self
]

{ #category : #generated }
DartForStatementNode >> assignableExpressions [

	^ assignableExpressions
]

{ #category : #generated }
DartForStatementNode >> assignableExpressions: anOrderedCollection [

	self setParents: self assignableExpressions to: nil.
	assignableExpressions := anOrderedCollection.
	self setParents: self assignableExpressions to: self
]

{ #category : #generated }
DartForStatementNode >> assignedExpressions [

	^ assignedExpressions
]

{ #category : #generated }
DartForStatementNode >> assignedExpressions: anOrderedCollection [

	self setParent: self assignedExpressions to: nil.
	assignedExpressions := anOrderedCollection.
	self setParent: self assignedExpressions to: self
]

{ #category : #generated }
DartForStatementNode >> assignmentOperators [

	^ assignmentOperators
]

{ #category : #generated }
DartForStatementNode >> assignmentOperators: anOrderedCollection [

	self setParents: self assignmentOperators to: nil.
	assignmentOperators := anOrderedCollection.
	self setParents: self assignmentOperators to: self
]

{ #category : #generated }
DartForStatementNode >> cascades [

	^ cascades
]

{ #category : #generated }
DartForStatementNode >> cascades: anOrderedCollection [

	self setParents: self cascades to: nil.
	cascades := anOrderedCollection.
	self setParents: self cascades to: self
]

{ #category : #generated }
DartForStatementNode >> compositeNodeVariables [

	^ #( #assignableExpressions #assignmentOperators #conditionalExpressions
	     #cascades #throwExpressions )
]

{ #category : #generated }
DartForStatementNode >> conditionalExpressions [

	^ conditionalExpressions
]

{ #category : #generated }
DartForStatementNode >> conditionalExpressions: anOrderedCollection [

	self setParents: self conditionalExpressions to: nil.
	conditionalExpressions := anOrderedCollection.
	self setParents: self conditionalExpressions to: self
]

{ #category : #generated }
DartForStatementNode >> declaredIdentifier [

	^ declaredIdentifier
]

{ #category : #generated }
DartForStatementNode >> declaredIdentifier: aDartIdentifierNode [

	self declaredIdentifier notNil ifTrue: [ 
		self declaredIdentifier parent: nil ].
	declaredIdentifier := aDartIdentifierNode.
	self declaredIdentifier notNil ifTrue: [ 
		self declaredIdentifier parent: self ]
]

{ #category : #generated }
DartForStatementNode >> finalConstVarOrType [

	^ finalConstVarOrType
]

{ #category : #generated }
DartForStatementNode >> finalConstVarOrType: anObject [

	self setParent: self finalConstVarOrType to: nil.
	finalConstVarOrType := anObject.
	self setParent: self finalConstVarOrType to: self
]

{ #category : #generated }
DartForStatementNode >> firstExpression [

	^ firstExpression
]

{ #category : #generated }
DartForStatementNode >> firstExpression: anOrderedCollection [

	self setParent: self firstExpression to: nil.
	firstExpression := anOrderedCollection.
	self setParent: self firstExpression to: self
]

{ #category : #generated }
DartForStatementNode >> function [

	^ function
]

{ #category : #generated }
DartForStatementNode >> function: aSmaCCToken [

	function := aSmaCCToken
]

{ #category : #generated }
DartForStatementNode >> idNotFunction [

	^ idNotFunction
]

{ #category : #generated }
DartForStatementNode >> idNotFunction: aSmaCCToken [

	idNotFunction := aSmaCCToken
]

{ #category : #'generated-initialize-release' }
DartForStatementNode >> initialize [
	super initialize.
	assignableExpressions := OrderedCollection new: 2.
	assignmentOperators := OrderedCollection new: 2.
	assignedExpressions := OrderedCollection new: 2.
	conditionalExpressions := OrderedCollection new: 2.
	cascades := OrderedCollection new: 2.
	throwExpressions := OrderedCollection new: 2.
	nextExpressions := OrderedCollection new: 2.
]

{ #category : #generated }
DartForStatementNode >> initializedVariableDeclaration [

	^ initializedVariableDeclaration
]

{ #category : #generated }
DartForStatementNode >> initializedVariableDeclaration: aDartInitializedVariableDeclarationNode [

	self initializedVariableDeclaration notNil ifTrue: [ 
		self initializedVariableDeclaration parent: nil ].
	initializedVariableDeclaration := aDartInitializedVariableDeclarationNode.
	self initializedVariableDeclaration notNil ifTrue: [ 
		self initializedVariableDeclaration parent: self ]
]

{ #category : #generated }
DartForStatementNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartForStatementNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartForStatementNode >> nextExpressions [

	^ nextExpressions
]

{ #category : #generated }
DartForStatementNode >> nextExpressions: anOrderedCollection [

	self setParent: self nextExpressions to: nil.
	nextExpressions := anOrderedCollection.
	self setParent: self nextExpressions to: self
]

{ #category : #generated }
DartForStatementNode >> nodeVariables [

	^ #( #declaredIdentifier #initializedVariableDeclaration
	     #statement )
]

{ #category : #generated }
DartForStatementNode >> otherVariables [

	^ #( #finalConstVarOrType #assignedExpressions #metadata #firstExpression
	     #nextExpressions )
]

{ #category : #generated }
DartForStatementNode >> statement [

	^ statement
]

{ #category : #generated }
DartForStatementNode >> statement: aDartStatementsNode [

	self statement notNil ifTrue: [ self statement parent: nil ].
	statement := aDartStatementsNode.
	self statement notNil ifTrue: [ self statement parent: self ]
]

{ #category : #generated }
DartForStatementNode >> throwExpressions [

	^ throwExpressions
]

{ #category : #generated }
DartForStatementNode >> throwExpressions: anOrderedCollection [

	self setParents: self throwExpressions to: nil.
	throwExpressions := anOrderedCollection.
	self setParents: self throwExpressions to: self
]

{ #category : #generated }
DartForStatementNode >> tokenVariables [

	^ #( #idNotFunction #function )
]
