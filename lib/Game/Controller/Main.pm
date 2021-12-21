package Game::Controller::Main;

use Moo;
use Game::Config;

use header;

extends 'Mojolicious::Controller';

sub main_page ($self)
{
	$self->render_lang('main/main_page');

	return;
}

sub set_lang ($self)
{
	my $lang = $self->param('lang');
	if (any { $_ eq $lang } Game::Config->supported_langs->@*) {
		$self->session->{lang} = $lang;
		# TODO: referrer
		$self->redirect_to('/');
	}
	else {
		$self->render(text => "Language $lang is not supported");
		$self->rendered(400);
	}

	return;
}

sub play ($self)
{
}

