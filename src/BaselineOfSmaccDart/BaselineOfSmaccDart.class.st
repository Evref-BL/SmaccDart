Class {
	#name : #BaselineOfSmaccDart,
	#superclass : #BaselineOf,
	#category : #BaselineOfSmaccDart
}

{ #category : #baselines }
BaselineOfSmaccDart >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			self defineDependencies: spec.
			self definePackages: spec.
			self defineGroups: spec ]
]

{ #category : #baselines }
BaselineOfSmaccDart >> defineDependencies: spec [
	spec
		baseline: 'SmaCC'
		with: [ spec
				repository: 'github://j-brant/SmaCC/src' ].
]

{ #category : #baselines }
BaselineOfSmaccDart >> defineGroups: spec [
]

{ #category : #baselines }
BaselineOfSmaccDart >> definePackages: spec [

	spec
		package: 'SmaCC_Dart' with: [ spec requires: #( 'SmaCC' ) ];
		package: 'SmaCC_Spec_UI' with: [ spec requires: #( 'SmaCC' ) ];
		package: 'SmaCC_Dart_Extension' with: [ spec requires: #( 'SmaCC_Dart' ) ]
]
