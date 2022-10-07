Class {
	#name : #DartConstructorNameNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeIdentifiers'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConstructorNameNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConstructorName: self
]

{ #category : #'generated-initialize-release' }
DartConstructorNameNode >> initialize [
	super initialize.
	typeIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartConstructorNameNode >> otherVariables [

	^ #( #typeIdentifiers )
]

{ #category : #generated }
DartConstructorNameNode >> typeIdentifiers [

	^ typeIdentifiers
]

{ #category : #generated }
DartConstructorNameNode >> typeIdentifiers: anOrderedCollection [

	self setParent: self typeIdentifiers to: nil.
	typeIdentifiers := anOrderedCollection.
	self setParent: self typeIdentifiers to: self
]
