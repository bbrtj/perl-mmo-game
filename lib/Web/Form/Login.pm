package Web::Form::Login;

use My::Moose;
use My::Form::Tiny -filtered, plugins => ['+Web::HTMLFormPlugin'];

use header;

extends 'Form::Login';

form_trim_strings;

form_field 'remember_me' => (
	type => Types::Bool,
	default => sub { 0 },
	data => {t => 'checkbox', values => [_tt('1:[_1]', _t('remember_me'))], l => undef},
);

form_hook after_validate => sub ($self, $data) {

	# clear password field, so that it won't end up in user's HTML (for their security)
	$self->input->{password} = '';
};

