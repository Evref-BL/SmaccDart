Class {
	#name : #DartLabelNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'LabelIdentifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLabelNode >> LabelIdentifier [

	^ LabelIdentifier
]

{ #category : #generated }
DartLabelNode >> LabelIdentifier: aDartIdentifierNode [

	self LabelIdentifier notNil ifTrue: [ 
		self LabelIdentifier parent: nil ].
	LabelIdentifier := aDartIdentifierNode.
	self LabelIdentifier notNil ifTrue: [ 
		self LabelIdentifier parent: self ]
]

{ #category : #generated }
DartLabelNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLabel: self
]

{ #category : #generated }
DartLabelNode >> nodeVariables [

	^ #( #LabelIdentifier )
]
