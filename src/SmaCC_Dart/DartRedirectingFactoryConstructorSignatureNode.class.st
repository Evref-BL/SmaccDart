Class {
	#name : #DartRedirectingFactoryConstructorSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'constructorName',
		'formalParameterList',
		'typeIdentifiers',
		'identifier',
		'typeName',
		'typeArguments'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitRedirectingFactoryConstructorSignature: self
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> compositeTokenVariables [

	^ #( #typeIdentifiers )
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> constructorName [

	^ constructorName
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> constructorName: aDartConstructorNameNode [

	self constructorName notNil ifTrue: [ 
		self constructorName parent: nil ].
	constructorName := aDartConstructorNameNode.
	self constructorName notNil ifTrue: [ 
		self constructorName parent: self ]
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> formalParameterList [

	^ formalParameterList
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> formalParameterList: anObject [

	self setParent: self formalParameterList to: nil.
	formalParameterList := anObject.
	self setParent: self formalParameterList to: self
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #'generated-initialize-release' }
DartRedirectingFactoryConstructorSignatureNode >> initialize [
	super initialize.
	typeIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> nodeVariables [

	^ #( #constructorName #identifier #typeName #typeArguments )
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> otherVariables [

	^ #( #formalParameterList )
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> typeArguments [

	^ typeArguments
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> typeArguments: aDartTypeListNode [

	self typeArguments notNil ifTrue: [ self typeArguments parent: nil ].
	typeArguments := aDartTypeListNode.
	self typeArguments notNil ifTrue: [ self typeArguments parent: self ]
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> typeIdentifiers [

	^ typeIdentifiers
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> typeIdentifiers: anOrderedCollection [

	typeIdentifiers := anOrderedCollection
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> typeName [

	^ typeName
]

{ #category : #generated }
DartRedirectingFactoryConstructorSignatureNode >> typeName: aDartTypeNameNode [

	self typeName notNil ifTrue: [ self typeName parent: nil ].
	typeName := aDartTypeNameNode.
	self typeName notNil ifTrue: [ self typeName parent: self ]
]
