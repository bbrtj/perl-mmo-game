package Server::Role::Listening;

use My::Moose::Role;

use header;

has field '_listeners' => (
	isa => Types::ArrayRef [Types::CodeRef],
	default => sub { [] },
	'handles[]' => {
		'_all_listeners' => 'all',
	},
);

sub _listen ($self, $channel, $id, $handler)
{
	$channel->listen($id, $handler);
	push $self->_listeners->@*, sub { $channel->unlisten($id) };

	return;
}

sub _unlisten ($self)
{
	$_->() foreach $self->_all_listeners;

	return;
}

