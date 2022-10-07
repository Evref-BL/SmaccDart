Class {
	#name : #DartConstructorSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'constructorName',
		'formalParameterList'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConstructorSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConstructorSignature: self
]

{ #category : #generated }
DartConstructorSignatureNode >> constructorName [

	^ constructorName
]

{ #category : #generated }
DartConstructorSignatureNode >> constructorName: aDartConstructorNameNode [

	self constructorName notNil ifTrue: [ 
		self constructorName parent: nil ].
	constructorName := aDartConstructorNameNode.
	self constructorName notNil ifTrue: [ 
		self constructorName parent: self ]
]

{ #category : #generated }
DartConstructorSignatureNode >> formalParameterList [

	^ formalParameterList
]

{ #category : #generated }
DartConstructorSignatureNode >> formalParameterList: anObject [

	self setParent: self formalParameterList to: nil.
	formalParameterList := anObject.
	self setParent: self formalParameterList to: self
]

{ #category : #generated }
DartConstructorSignatureNode >> nodeVariables [

	^ #( #constructorName )
]

{ #category : #generated }
DartConstructorSignatureNode >> otherVariables [

	^ #( #formalParameterList )
]
