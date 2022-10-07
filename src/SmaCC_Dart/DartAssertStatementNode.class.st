Class {
	#name : #DartAssertStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'assertion'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAssertStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAssertStatement: self
]

{ #category : #generated }
DartAssertStatementNode >> assertion [

	^ assertion
]

{ #category : #generated }
DartAssertStatementNode >> assertion: aDartAssertionNode [

	self assertion notNil ifTrue: [ self assertion parent: nil ].
	assertion := aDartAssertionNode.
	self assertion notNil ifTrue: [ self assertion parent: self ]
]

{ #category : #generated }
DartAssertStatementNode >> nodeVariables [

	^ #( #assertion )
]
