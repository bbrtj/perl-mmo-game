package Web::Controller::Main;

use My::Moose;
use Web::Config;
use Future::AsyncAwait;

use header;

extends 'Web::Controller';

sub build ($self)
{
	my $global_bridge = $self->router->find('global_bridge');

	$global_bridge->add('/' => {
		to => 'main_page',
		name => 'main_page',
	});

	$global_bridge->add('/lang/:lang' => {
		to => 'set_lang',
		name => 'set_lang',
	});
}

sub main_page ($self, $ctx)
{
	return $self->template_lang($ctx, 'main/main_page', {
		user => $ctx->stash->get('user'),
	});
}

async sub set_lang ($self, $ctx, $lang)
{
	if (
		any { $_ eq $lang }
		Web::Config->supported_langs->@*
		)
	{
		$ctx->session->set('lang', $lang);

		# TODO: referrer
		await $ctx->res->redirect($self->url_for('main_page'));
	}
	else {
		await $ctx->res->status(400)->text("Language $lang is not supported");
	}
}

