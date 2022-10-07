Class {
	#name : #DartElementsNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'element',
		'nextElementses'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartElementsNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitElements: self
]

{ #category : #generated }
DartElementsNode >> compositeNodeVariables [

	^ #( #nextElementses )
]

{ #category : #generated }
DartElementsNode >> element [

	^ element
]

{ #category : #generated }
DartElementsNode >> element: aDartElementNode [

	self element notNil ifTrue: [ self element parent: nil ].
	element := aDartElementNode.
	self element notNil ifTrue: [ self element parent: self ]
]

{ #category : #'generated-initialize-release' }
DartElementsNode >> initialize [
	super initialize.
	nextElementses := OrderedCollection new: 2.
]

{ #category : #generated }
DartElementsNode >> nextElementses [

	^ nextElementses
]

{ #category : #generated }
DartElementsNode >> nextElementses: anOrderedCollection [

	self setParents: self nextElementses to: nil.
	nextElementses := anOrderedCollection.
	self setParents: self nextElementses to: self
]

{ #category : #generated }
DartElementsNode >> nodeVariables [

	^ #( #element )
]
