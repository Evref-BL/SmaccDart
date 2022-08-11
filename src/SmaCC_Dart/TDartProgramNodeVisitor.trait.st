Trait {
	#name : #TDartProgramNodeVisitor,
	#traits : 'TSmaCCParseNodeVisitor',
	#classTraits : 'TSmaCCParseNodeVisitor classTrait',
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
TDartProgramNodeVisitor >> visitClassDeclaration: aClassDeclaration [

	^ self visitProgram: aClassDeclaration
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitClassMemberDefinition: aClassMemberDefinition [

	^ self visitProgram: aClassMemberDefinition
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitLibraryDefinition: aLibraryDefinition [

	^ self visitProgram: aLibraryDefinition
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitProgram: aProgram [

	^ self visitSmaCCParseNode: aProgram
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitType: aType [

	^ self visitProgram: aType
]
