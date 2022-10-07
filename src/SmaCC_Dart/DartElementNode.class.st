Class {
	#name : #DartElementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'expressionElement',
		'mapElement',
		'spreadElement',
		'ifElement',
		'forElement'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartElementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitElement: self
]

{ #category : #generated }
DartElementNode >> expressionElement [

	^ expressionElement
]

{ #category : #generated }
DartElementNode >> expressionElement: aDartExpressionElementNode [

	self expressionElement notNil ifTrue: [ 
		self expressionElement parent: nil ].
	expressionElement := aDartExpressionElementNode.
	self expressionElement notNil ifTrue: [ 
		self expressionElement parent: self ]
]

{ #category : #generated }
DartElementNode >> forElement [

	^ forElement
]

{ #category : #generated }
DartElementNode >> forElement: anOrderedCollection [

	self setParent: self forElement to: nil.
	forElement := anOrderedCollection.
	self setParent: self forElement to: self
]

{ #category : #generated }
DartElementNode >> ifElement [

	^ ifElement
]

{ #category : #generated }
DartElementNode >> ifElement: anOrderedCollection [

	self setParent: self ifElement to: nil.
	ifElement := anOrderedCollection.
	self setParent: self ifElement to: self
]

{ #category : #generated }
DartElementNode >> mapElement [

	^ mapElement
]

{ #category : #generated }
DartElementNode >> mapElement: anOrderedCollection [

	self setParent: self mapElement to: nil.
	mapElement := anOrderedCollection.
	self setParent: self mapElement to: self
]

{ #category : #generated }
DartElementNode >> nodeVariables [

	^ #( #expressionElement )
]

{ #category : #generated }
DartElementNode >> otherVariables [

	^ #( #mapElement #spreadElement #ifElement #forElement )
]

{ #category : #generated }
DartElementNode >> spreadElement [

	^ spreadElement
]

{ #category : #generated }
DartElementNode >> spreadElement: anOrderedCollection [

	self setParent: self spreadElement to: nil.
	spreadElement := anOrderedCollection.
	self setParent: self spreadElement to: self
]
