import { Component, ComponentRef, ComponentFactoryResolver, ViewContainerRef, ViewChild } from '@angular/core';

import { CharacterSelectionComponent } from './character-selection/character-selection.component';
import { MapComponent } from './map/map.component';
import { ComponentMap } from './component-map';

@Component({
	selector: 'game-root',
	templateUrl: './app.component.html',
	styleUrls: ['./app.component.scss']
})
export class AppComponent {
	title = 'game';
	private components: ComponentMap;

	@ViewChild('componentPlaceholder', { read: ViewContainerRef, static: true })
	public componentPlaceholder: ViewContainerRef;

	constructor(containerRef: ViewContainerRef) {
		this.componentPlaceholder = containerRef;
		this.components = {};
	}

	showCharacters() {
		this.switchComponent(CharacterSelectionComponent);
	}

	showMap() {
		this.switchComponent(MapComponent);
	}

	switchComponent(type: any) {
		const typestr = type.componentName();

		this.componentPlaceholder.detach();
		if (typestr in this.components) {
			this.componentPlaceholder.insert(this.components[typestr].hostView);
		}
		else {
			let ref: ComponentRef<any>;
			ref = this.componentPlaceholder.createComponent(type);
			this.components[typestr] = ref;
		}
		this.components[typestr].instance.called();
	}
}
