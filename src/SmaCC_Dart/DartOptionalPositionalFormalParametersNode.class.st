Class {
	#name : #DartOptionalPositionalFormalParametersNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'defaultFormalParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitOptionalPositionalFormalParameters: self
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> compositeNodeVariables [

	^ #( #defaultFormalParameters )
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> defaultFormalParameters [

	^ defaultFormalParameters
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> defaultFormalParameters: anOrderedCollection [

	self setParents: self defaultFormalParameters to: nil.
	defaultFormalParameters := anOrderedCollection.
	self setParents: self defaultFormalParameters to: self
]

{ #category : #'generated-initialize-release' }
DartOptionalPositionalFormalParametersNode >> initialize [
	super initialize.
	defaultFormalParameters := OrderedCollection new: 2.
]
