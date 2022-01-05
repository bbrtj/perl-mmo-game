package Web::Controller::Main;

use My::Moose -constr;
use Web::Config;
use DI;

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
	if (
		any { $_ eq $lang }
		Web::Config->supported_langs->@*
		)
	{
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
	my $lang = $self->stash('lang');
	$self->reply->static("play/$lang/index.html");

	return;
}

