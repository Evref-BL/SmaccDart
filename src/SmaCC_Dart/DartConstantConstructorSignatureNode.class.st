Class {
	#name : #DartConstantConstructorSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'constructorName',
		'formalParameterList'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConstantConstructorSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConstantConstructorSignature: self
]

{ #category : #generated }
DartConstantConstructorSignatureNode >> constructorName [

	^ constructorName
]

{ #category : #generated }
DartConstantConstructorSignatureNode >> constructorName: aDartConstructorNameNode [

	self constructorName notNil ifTrue: [ 
		self constructorName parent: nil ].
	constructorName := aDartConstructorNameNode.
	self constructorName notNil ifTrue: [ 
		self constructorName parent: self ]
]

{ #category : #generated }
DartConstantConstructorSignatureNode >> formalParameterList [

	^ formalParameterList
]

{ #category : #generated }
DartConstantConstructorSignatureNode >> formalParameterList: aDartProgramNode [

	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: nil ].
	formalParameterList := aDartProgramNode.
	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: self ]
]

{ #category : #generated }
DartConstantConstructorSignatureNode >> nodeVariables [

	^ #( #constructorName #formalParameterList )
]
