Class {
	#name : #DartAssertionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'expressions'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAssertionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAssertion: self
]

{ #category : #generated }
DartAssertionNode >> expressions [

	^ expressions
]

{ #category : #generated }
DartAssertionNode >> expressions: anOrderedCollection [

	self setParent: self expressions to: nil.
	expressions := anOrderedCollection.
	self setParent: self expressions to: self
]

{ #category : #'generated-initialize-release' }
DartAssertionNode >> initialize [
	super initialize.
	expressions := OrderedCollection new: 2.
]

{ #category : #generated }
DartAssertionNode >> otherVariables [

	^ #( #expressions )
]
