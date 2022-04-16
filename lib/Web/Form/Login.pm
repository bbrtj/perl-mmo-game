package Web::Form::Login;

use Form::Tiny -filtered, plugins => ['+Web::HTMLFormPlugin'];
use DI;
use Types;

use header;

extends 'Form::Login';

form_trim_strings;

form_field 'remember_me' => (
	type => Types::Bool,
	default => sub { 0 },
	data => {t => 'checkbox', values => [_tt('1:[_1]', _t('remember_me'))], l => undef},
);

