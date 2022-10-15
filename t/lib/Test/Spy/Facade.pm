package Test::Spy::Facade;

use My::Moose::Role;

use header;

has option 'context' => (
	writer => 1,
);

with qw(Test::Spy::Interface);

sub _get_context ($self)
{
	croak 'no context was set in ' . ref $self
		unless $self->has_context;

	return $self->context;
}

sub called_times ($self, @args)
{
	return $self->_get_context->called_times(@args);
}

sub call_history ($self, @args)
{
	return $self->_get_context->call_history(@args);
}

sub called_with ($self, @args)
{
	return $self->_get_context->called_with(@args);
}

sub first_called_with ($self, @args)
{
	return $self->_get_context->first_called_with(@args);
}

sub next_called_with ($self, @args)
{
	return $self->_get_context->next_called_with(@args);
}

sub was_called ($self, @args)
{
	return $self->_get_context->was_called(@args);
}

sub was_called_once ($self, @args)
{
	return $self->_get_context->was_called_once(@args);
}

sub clear ($self, @args)
{
	return $self->_get_context->clear(@args);
}

