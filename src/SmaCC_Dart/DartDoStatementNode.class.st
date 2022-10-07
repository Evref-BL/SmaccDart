Class {
	#name : #DartDoStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'doStatement',
		'doConditionExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartDoStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitDoStatement: self
]

{ #category : #generated }
DartDoStatementNode >> doConditionExpression [

	^ doConditionExpression
]

{ #category : #generated }
DartDoStatementNode >> doConditionExpression: anOrderedCollection [

	self setParent: self doConditionExpression to: nil.
	doConditionExpression := anOrderedCollection.
	self setParent: self doConditionExpression to: self
]

{ #category : #generated }
DartDoStatementNode >> doStatement [

	^ doStatement
]

{ #category : #generated }
DartDoStatementNode >> doStatement: aDartSingleStatementNode [

	self doStatement notNil ifTrue: [ self doStatement parent: nil ].
	doStatement := aDartSingleStatementNode.
	self doStatement notNil ifTrue: [ self doStatement parent: self ]
]

{ #category : #generated }
DartDoStatementNode >> nodeVariables [

	^ #( #doStatement )
]

{ #category : #generated }
DartDoStatementNode >> otherVariables [

	^ #( #doConditionExpression )
]
