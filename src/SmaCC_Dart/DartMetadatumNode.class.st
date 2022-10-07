Class {
	#name : #DartMetadatumNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'constructorDesignation',
		'arguments'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartMetadatumNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitMetadatum: self
]

{ #category : #generated }
DartMetadatumNode >> arguments [

	^ arguments
]

{ #category : #generated }
DartMetadatumNode >> arguments: anObject [

	self setParent: self arguments to: nil.
	arguments := anObject.
	self setParent: self arguments to: self
]

{ #category : #generated }
DartMetadatumNode >> constructorDesignation [

	^ constructorDesignation
]

{ #category : #generated }
DartMetadatumNode >> constructorDesignation: anObject [

	self setParent: self constructorDesignation to: nil.
	constructorDesignation := anObject.
	self setParent: self constructorDesignation to: self
]

{ #category : #generated }
DartMetadatumNode >> otherVariables [

	^ #( #constructorDesignation #arguments )
]
