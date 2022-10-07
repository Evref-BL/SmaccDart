Class {
	#name : #DartTypeParametersNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeParameter',
		'nextTypeParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypeParametersNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTypeParameters: self
]

{ #category : #generated }
DartTypeParametersNode >> compositeNodeVariables [

	^ #( #nextTypeParameters )
]

{ #category : #'generated-initialize-release' }
DartTypeParametersNode >> initialize [
	super initialize.
	nextTypeParameters := OrderedCollection new: 2.
]

{ #category : #generated }
DartTypeParametersNode >> nextTypeParameters [

	^ nextTypeParameters
]

{ #category : #generated }
DartTypeParametersNode >> nextTypeParameters: anOrderedCollection [

	self setParents: self nextTypeParameters to: nil.
	nextTypeParameters := anOrderedCollection.
	self setParents: self nextTypeParameters to: self
]

{ #category : #generated }
DartTypeParametersNode >> nodeVariables [

	^ #( #typeParameter )
]

{ #category : #generated }
DartTypeParametersNode >> typeParameter [

	^ typeParameter
]

{ #category : #generated }
DartTypeParametersNode >> typeParameter: aDartTypeParameterNode [

	self typeParameter notNil ifTrue: [ self typeParameter parent: nil ].
	typeParameter := aDartTypeParameterNode.
	self typeParameter notNil ifTrue: [ self typeParameter parent: self ]
]
