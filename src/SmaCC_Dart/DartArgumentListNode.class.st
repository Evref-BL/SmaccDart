Class {
	#name : #DartArgumentListNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'namedArguments',
		'expressionList'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartArgumentListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitArgumentList: self
]

{ #category : #generated }
DartArgumentListNode >> compositeNodeVariables [

	^ #( #namedArguments )
]

{ #category : #generated }
DartArgumentListNode >> expressionList [

	^ expressionList
]

{ #category : #generated }
DartArgumentListNode >> expressionList: aDartExpressionListNode [

	self expressionList notNil ifTrue: [ self expressionList parent: nil ].
	expressionList := aDartExpressionListNode.
	self expressionList notNil ifTrue: [ 
		self expressionList parent: self ]
]

{ #category : #'generated-initialize-release' }
DartArgumentListNode >> initialize [
	super initialize.
	namedArguments := OrderedCollection new: 2.
]

{ #category : #generated }
DartArgumentListNode >> namedArguments [

	^ namedArguments
]

{ #category : #generated }
DartArgumentListNode >> namedArguments: anOrderedCollection [

	self setParents: self namedArguments to: nil.
	namedArguments := anOrderedCollection.
	self setParents: self namedArguments to: self
]

{ #category : #generated }
DartArgumentListNode >> nodeVariables [

	^ #( #expressionList )
]
