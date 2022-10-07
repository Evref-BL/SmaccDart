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
DartFactoryConstructorSignatureNode >> formalParameterList: anObject [

	self setParent: self formalParameterList to: nil.
	formalParameterList := anObject.
	self setParent: self formalParameterList to: self
]

{ #category : #generated }
DartFactoryConstructorSignatureNode >> nodeVariables [

	^ #( #constructorName )
]

{ #category : #generated }
DartFactoryConstructorSignatureNode >> otherVariables [

	^ #( #formalParameterList )
]
