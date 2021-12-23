import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { AppComponent } from './app.component';
import { CharacterSelectionComponent } from './character-selection/character-selection.component';
import { MapComponent } from './map/map.component';

@NgModule({
	declarations: [
		AppComponent,
		CharacterSelectionComponent,
		MapComponent,
	],
	imports: [
		BrowserModule,
	],
	providers: [],
	bootstrap: [AppComponent]
})
export class AppModule {
}
