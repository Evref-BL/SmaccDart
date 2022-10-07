Class {
	#name : #DartUriTestNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'dottedIdentifierList',
		'stringLiteral'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartUriTestNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitUriTest: self
]

{ #category : #generated }
DartUriTestNode >> dottedIdentifierList [

	^ dottedIdentifierList
]

{ #category : #generated }
DartUriTestNode >> dottedIdentifierList: aDartDottedIdentifierListNode [

	self dottedIdentifierList notNil ifTrue: [ 
		self dottedIdentifierList parent: nil ].
	dottedIdentifierList := aDartDottedIdentifierListNode.
	self dottedIdentifierList notNil ifTrue: [ 
		self dottedIdentifierList parent: self ]
]

{ #category : #generated }
DartUriTestNode >> nodeVariables [

	^ #( #dottedIdentifierList #stringLiteral )
]

{ #category : #generated }
DartUriTestNode >> stringLiteral [

	^ stringLiteral
]

{ #category : #generated }
DartUriTestNode >> stringLiteral: aDartStringLiteralNode [

	self stringLiteral notNil ifTrue: [ self stringLiteral parent: nil ].
	stringLiteral := aDartStringLiteralNode.
	self stringLiteral notNil ifTrue: [ self stringLiteral parent: self ]
]
