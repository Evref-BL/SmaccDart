Class {
	#name : #DartMixinApplicationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeNotVoidNotFunction',
		'mixins',
		'interfaces'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartMixinApplicationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitMixinApplication: self
]

{ #category : #generated }
DartMixinApplicationNode >> interfaces [

	^ interfaces
]

{ #category : #generated }
DartMixinApplicationNode >> interfaces: aDartTypeNotVoidNotFunctionListNode [

	self interfaces notNil ifTrue: [ self interfaces parent: nil ].
	interfaces := aDartTypeNotVoidNotFunctionListNode.
	self interfaces notNil ifTrue: [ self interfaces parent: self ]
]

{ #category : #generated }
DartMixinApplicationNode >> mixins [

	^ mixins
]

{ #category : #generated }
DartMixinApplicationNode >> mixins: aDartTypeNotVoidNotFunctionListNode [

	self mixins notNil ifTrue: [ self mixins parent: nil ].
	mixins := aDartTypeNotVoidNotFunctionListNode.
	self mixins notNil ifTrue: [ self mixins parent: self ]
]

{ #category : #generated }
DartMixinApplicationNode >> nodeVariables [

	^ #( #mixins #interfaces )
]

{ #category : #generated }
DartMixinApplicationNode >> otherVariables [

	^ #( #typeNotVoidNotFunction )
]

{ #category : #generated }
DartMixinApplicationNode >> typeNotVoidNotFunction [

	^ typeNotVoidNotFunction
]

{ #category : #generated }
DartMixinApplicationNode >> typeNotVoidNotFunction: anObject [

	self setParent: self typeNotVoidNotFunction to: nil.
	typeNotVoidNotFunction := anObject.
	self setParent: self typeNotVoidNotFunction to: self
]
