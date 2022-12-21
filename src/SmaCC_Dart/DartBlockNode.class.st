Class {
	#name : #DartBlockNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'statements'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartBlockNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitBlock: self
]

{ #category : #generated }
DartBlockNode >> nodeVariables [

	^ #( #statements )
]

{ #category : #generated }
DartBlockNode >> statements [

	^ statements
]

{ #category : #generated }
DartBlockNode >> statements: aDartStatementsNode [

	self statements notNil ifTrue: [ self statements parent: nil ].
	statements := aDartStatementsNode.
	self statements notNil ifTrue: [ self statements parent: self ]
]
