Class {
	#name : #DartFactoryConstructorSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'constructorName',
		'formalParameterList'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFactoryConstructorSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFactoryConstructorSignature: self
]

{ #category : #generated }
DartFactoryConstructorSignatureNode >> constructorName [

	^ constructorName
]

{ #category : #generated }
DartFactoryConstructorSignatureNode >> constructorName: aDartConstructorNameNode [

	self constructorName notNil ifTrue: [ 
		self constructorName parent: nil ].
	constructorName := aDartConstructorNameNode.
	self constructorName notNil ifTrue: [ 
		self constructorName parent: self ]
]

{ #category : #generated }
DartFactoryConstructorSignatureNode >> formalParameterList [

	^ formalParameterList
]

{ #category : #generated }
DartFactoryConstructorSignatureNode >> formalParameterList: aDartProgramNode [

	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: nil ].
	formalParameterList := aDartProgramNode.
	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: self ]
]

{ #category : #generated }
DartFactoryConstructorSignatureNode >> nodeVariables [

	^ #( #constructorName #formalParameterList )
]
