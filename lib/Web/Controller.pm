package Web::Controller;

use My::Moose;

use header;

extends 'Thunderhorse::Controller';

sub template ($self, $name, $args = {})
{
	$args = {
		$args->%*,
		t => sub { _t(@_) },
		tt => sub { _tt(@_) },
		controller => $self,
	};

	return $self->SUPER::template($name, $args);
}

sub template_lang ($self, $ctx, @args)
{
	local $i18n::CURRENT_LANG = $ctx->stash->get('lang');
	return $self->template(@args);
}

