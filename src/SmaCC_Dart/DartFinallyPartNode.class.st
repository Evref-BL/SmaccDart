Class {
	#name : #DartFinallyPartNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'block'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFinallyPartNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFinallyPart: self
]

{ #category : #generated }
DartFinallyPartNode >> block [

	^ block
]

{ #category : #generated }
DartFinallyPartNode >> block: aDartBlockNode [

	self block notNil ifTrue: [ self block parent: nil ].
	block := aDartBlockNode.
	self block notNil ifTrue: [ self block parent: self ]
]

{ #category : #generated }
DartFinallyPartNode >> nodeVariables [

	^ #( #block )
]
