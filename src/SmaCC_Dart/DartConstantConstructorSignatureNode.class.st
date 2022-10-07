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
DartConstantConstructorSignatureNode >> formalParameterList: anObject [

	self setParent: self formalParameterList to: nil.
	formalParameterList := anObject.
	self setParent: self formalParameterList to: self
]

{ #category : #generated }
DartConstantConstructorSignatureNode >> nodeVariables [

	^ #( #constructorName )
]

{ #category : #generated }
DartConstantConstructorSignatureNode >> otherVariables [

	^ #( #formalParameterList )
]
