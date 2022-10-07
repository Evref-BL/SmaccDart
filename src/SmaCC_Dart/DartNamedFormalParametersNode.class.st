Class {
	#name : #DartNamedFormalParametersNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'defaultNamedParameter',
		'nextDefaultNamedParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNamedFormalParametersNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNamedFormalParameters: self
]

{ #category : #generated }
DartNamedFormalParametersNode >> compositeNodeVariables [

	^ #( #nextDefaultNamedParameters )
]

{ #category : #generated }
DartNamedFormalParametersNode >> defaultNamedParameter [

	^ defaultNamedParameter
]

{ #category : #generated }
DartNamedFormalParametersNode >> defaultNamedParameter: aDartNormalFormalParameterNode [

	self defaultNamedParameter notNil ifTrue: [ 
		self defaultNamedParameter parent: nil ].
	defaultNamedParameter := aDartNormalFormalParameterNode.
	self defaultNamedParameter notNil ifTrue: [ 
		self defaultNamedParameter parent: self ]
]

{ #category : #'generated-initialize-release' }
DartNamedFormalParametersNode >> initialize [
	super initialize.
	nextDefaultNamedParameters := OrderedCollection new: 2.
]

{ #category : #generated }
DartNamedFormalParametersNode >> nextDefaultNamedParameters [

	^ nextDefaultNamedParameters
]

{ #category : #generated }
DartNamedFormalParametersNode >> nextDefaultNamedParameters: anOrderedCollection [

	self setParents: self nextDefaultNamedParameters to: nil.
	nextDefaultNamedParameters := anOrderedCollection.
	self setParents: self nextDefaultNamedParameters to: self
]

{ #category : #generated }
DartNamedFormalParametersNode >> nodeVariables [

	^ #( #defaultNamedParameter )
]
