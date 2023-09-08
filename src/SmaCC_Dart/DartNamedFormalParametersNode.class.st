Class {
	#name : #DartNamedFormalParametersNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'parameter',
		'nextParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNamedFormalParametersNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNamedFormalParameters: self
]

{ #category : #generated }
DartNamedFormalParametersNode >> compositeNodeVariables [

	^ #( #nextParameters )
]

{ #category : #'generated-initialize-release' }
DartNamedFormalParametersNode >> initialize [
	super initialize.
	nextParameters := OrderedCollection new: 2.
]

{ #category : #generated }
DartNamedFormalParametersNode >> nextParameters [

	^ nextParameters
]

{ #category : #generated }
DartNamedFormalParametersNode >> nextParameters: anOrderedCollection [

	self setParents: self nextParameters to: nil.
	nextParameters := anOrderedCollection.
	self setParents: self nextParameters to: self
]

{ #category : #generated }
DartNamedFormalParametersNode >> nodeVariables [

	^ #( #parameter )
]

{ #category : #generated }
DartNamedFormalParametersNode >> parameter [

	^ parameter
]

{ #category : #generated }
DartNamedFormalParametersNode >> parameter: aDartNormalFormalParameterNode [

	self parameter notNil ifTrue: [ self parameter parent: nil ].
	parameter := aDartNormalFormalParameterNode.
	self parameter notNil ifTrue: [ self parameter parent: self ]
]
