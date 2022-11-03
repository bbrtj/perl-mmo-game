package Server::Role::Listening;

use My::Moose::Role;

# use Sub::HandlesVia;

use header;

has field '_callbacks' => (
	isa => Types::ArrayRef [Types::CodeRef],
	default => sub { [] },
	'handles[]' => {
		'_all_callbacks' => 'all',
	},
);

sub _listen ($self, $channel, $id, $handler)
{
	my $wrapped = $channel->listen($id, $handler);
	push $self->_callbacks->@*, sub { $channel->unlisten($id, $wrapped) };

	return;
}

sub _unlisten ($self)
{
	$_->() foreach $self->_all_callbacks;

	return;
}

