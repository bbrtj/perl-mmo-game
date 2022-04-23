package CLI;

use My::Moose -constr;

use header;

extends 'Mojolicious';

sub startup ($self)
{
	push $self->commands->namespaces->@*, "CLI";

	return;
}

