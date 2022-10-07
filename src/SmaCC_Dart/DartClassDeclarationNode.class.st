Class {
	#name : #DartClassDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeWithParameters',
		'metadatas',
		'classMemberDefinitions',
		'interfaces',
		'mixins',
		'superclass',
		'mixinApplicationClass'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartClassDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitClassDeclaration: self
]

{ #category : #generated }
DartClassDeclarationNode >> classMemberDefinitions [

	^ classMemberDefinitions
]

{ #category : #generated }
DartClassDeclarationNode >> classMemberDefinitions: anOrderedCollection [

	self setParents: self classMemberDefinitions to: nil.
	classMemberDefinitions := anOrderedCollection.
	self setParents: self classMemberDefinitions to: self
]

{ #category : #generated }
DartClassDeclarationNode >> compositeNodeVariables [

	^ #( #classMemberDefinitions )
]

{ #category : #'generated-initialize-release' }
DartClassDeclarationNode >> initialize [
	super initialize.
	metadatas := OrderedCollection new: 2.
	classMemberDefinitions := OrderedCollection new: 2.
]

{ #category : #generated }
DartClassDeclarationNode >> interfaces [

	^ interfaces
]

{ #category : #generated }
DartClassDeclarationNode >> interfaces: aDartTypeNotVoidNotFunctionListNode [

	self interfaces notNil ifTrue: [ self interfaces parent: nil ].
	interfaces := aDartTypeNotVoidNotFunctionListNode.
	self interfaces notNil ifTrue: [ self interfaces parent: self ]
]

{ #category : #generated }
DartClassDeclarationNode >> metadatas [

	^ metadatas
]

{ #category : #generated }
DartClassDeclarationNode >> metadatas: anOrderedCollection [

	self setParent: self metadatas to: nil.
	metadatas := anOrderedCollection.
	self setParent: self metadatas to: self
]

{ #category : #generated }
DartClassDeclarationNode >> mixinApplicationClass [

	^ mixinApplicationClass
]

{ #category : #generated }
DartClassDeclarationNode >> mixinApplicationClass: aDartMixinApplicationClassNode [

	self mixinApplicationClass notNil ifTrue: [ 
		self mixinApplicationClass parent: nil ].
	mixinApplicationClass := aDartMixinApplicationClassNode.
	self mixinApplicationClass notNil ifTrue: [ 
		self mixinApplicationClass parent: self ]
]

{ #category : #generated }
DartClassDeclarationNode >> mixins [

	^ mixins
]

{ #category : #generated }
DartClassDeclarationNode >> mixins: aDartTypeNotVoidNotFunctionListNode [

	self mixins notNil ifTrue: [ self mixins parent: nil ].
	mixins := aDartTypeNotVoidNotFunctionListNode.
	self mixins notNil ifTrue: [ self mixins parent: self ]
]

{ #category : #generated }
DartClassDeclarationNode >> nodeVariables [

	^ #( #typeWithParameters #interfaces #mixins #mixinApplicationClass )
]

{ #category : #generated }
DartClassDeclarationNode >> otherVariables [

	^ #( #metadatas #superclass )
]

{ #category : #generated }
DartClassDeclarationNode >> superclass [

	^ superclass
]

{ #category : #generated }
DartClassDeclarationNode >> superclass: anObject [

	self setParent: self superclass to: nil.
	superclass := anObject.
	self setParent: self superclass to: self
]

{ #category : #generated }
DartClassDeclarationNode >> typeWithParameters [

	^ typeWithParameters
]

{ #category : #generated }
DartClassDeclarationNode >> typeWithParameters: aDartTypeNode [

	self typeWithParameters notNil ifTrue: [ 
		self typeWithParameters parent: nil ].
	typeWithParameters := aDartTypeNode.
	self typeWithParameters notNil ifTrue: [ 
		self typeWithParameters parent: self ]
]
