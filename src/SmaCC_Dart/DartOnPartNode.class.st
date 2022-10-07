Class {
	#name : #DartOnPartNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'catchPart',
		'block',
		'typeNotVoid'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartOnPartNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitOnPart: self
]

{ #category : #generated }
DartOnPartNode >> block [

	^ block
]

{ #category : #generated }
DartOnPartNode >> block: aDartBlockNode [

	self block notNil ifTrue: [ self block parent: nil ].
	block := aDartBlockNode.
	self block notNil ifTrue: [ self block parent: self ]
]

{ #category : #generated }
DartOnPartNode >> catchPart [

	^ catchPart
]

{ #category : #generated }
DartOnPartNode >> catchPart: aDartCatchPartNode [

	self catchPart notNil ifTrue: [ self catchPart parent: nil ].
	catchPart := aDartCatchPartNode.
	self catchPart notNil ifTrue: [ self catchPart parent: self ]
]

{ #category : #generated }
DartOnPartNode >> nodeVariables [

	^ #( #catchPart #block )
]

{ #category : #generated }
DartOnPartNode >> otherVariables [

	^ #( #typeNotVoid )
]

{ #category : #generated }
DartOnPartNode >> typeNotVoid [

	^ typeNotVoid
]

{ #category : #generated }
DartOnPartNode >> typeNotVoid: anObject [

	self setParent: self typeNotVoid to: nil.
	typeNotVoid := anObject.
	self setParent: self typeNotVoid to: self
]
