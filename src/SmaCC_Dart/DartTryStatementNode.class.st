Class {
	#name : #DartTryStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'block',
		'onParts',
		'finallyPart'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTryStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTryStatement: self
]

{ #category : #generated }
DartTryStatementNode >> block [

	^ block
]

{ #category : #generated }
DartTryStatementNode >> block: aDartBlockNode [

	self block notNil ifTrue: [ self block parent: nil ].
	block := aDartBlockNode.
	self block notNil ifTrue: [ self block parent: self ]
]

{ #category : #generated }
DartTryStatementNode >> finallyPart [

	^ finallyPart
]

{ #category : #generated }
DartTryStatementNode >> finallyPart: aDartFinallyPartNode [

	self finallyPart notNil ifTrue: [ self finallyPart parent: nil ].
	finallyPart := aDartFinallyPartNode.
	self finallyPart notNil ifTrue: [ self finallyPart parent: self ]
]

{ #category : #generated }
DartTryStatementNode >> nodeVariables [

	^ #( #block #onParts #finallyPart )
]

{ #category : #generated }
DartTryStatementNode >> onParts [

	^ onParts
]

{ #category : #generated }
DartTryStatementNode >> onParts: aDartOnPartNode [

	self onParts notNil ifTrue: [ self onParts parent: nil ].
	onParts := aDartOnPartNode.
	self onParts notNil ifTrue: [ self onParts parent: self ]
]
