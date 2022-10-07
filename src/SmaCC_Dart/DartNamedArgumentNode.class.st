Class {
	#name : #DartNamedArgumentNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'label',
		'expression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNamedArgumentNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNamedArgument: self
]

{ #category : #generated }
DartNamedArgumentNode >> expression [

	^ expression
]

{ #category : #generated }
DartNamedArgumentNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartNamedArgumentNode >> label [

	^ label
]

{ #category : #generated }
DartNamedArgumentNode >> label: aDartLabelNode [

	self label notNil ifTrue: [ self label parent: nil ].
	label := aDartLabelNode.
	self label notNil ifTrue: [ self label parent: self ]
]

{ #category : #generated }
DartNamedArgumentNode >> nodeVariables [

	^ #( #label )
]

{ #category : #generated }
DartNamedArgumentNode >> otherVariables [

	^ #( #expression )
]
