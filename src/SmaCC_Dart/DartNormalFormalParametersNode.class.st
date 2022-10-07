Class {
	#name : #DartNormalFormalParametersNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'normalFormalParameter',
		'nextNormalFormalParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNormalFormalParametersNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNormalFormalParameters: self
]

{ #category : #generated }
DartNormalFormalParametersNode >> compositeNodeVariables [

	^ #( #nextNormalFormalParameters )
]

{ #category : #'generated-initialize-release' }
DartNormalFormalParametersNode >> initialize [
	super initialize.
	nextNormalFormalParameters := OrderedCollection new: 2.
]

{ #category : #generated }
DartNormalFormalParametersNode >> nextNormalFormalParameters [

	^ nextNormalFormalParameters
]

{ #category : #generated }
DartNormalFormalParametersNode >> nextNormalFormalParameters: anOrderedCollection [

	self setParents: self nextNormalFormalParameters to: nil.
	nextNormalFormalParameters := anOrderedCollection.
	self setParents: self nextNormalFormalParameters to: self
]

{ #category : #generated }
DartNormalFormalParametersNode >> nodeVariables [

	^ #( #normalFormalParameter )
]

{ #category : #generated }
DartNormalFormalParametersNode >> normalFormalParameter [

	^ normalFormalParameter
]

{ #category : #generated }
DartNormalFormalParametersNode >> normalFormalParameter: aDartNormalFormalParameterNode [

	self normalFormalParameter notNil ifTrue: [ 
		self normalFormalParameter parent: nil ].
	normalFormalParameter := aDartNormalFormalParameterNode.
	self normalFormalParameter notNil ifTrue: [ 
		self normalFormalParameter parent: self ]
]
