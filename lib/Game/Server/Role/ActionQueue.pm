package Game::Server::Role::ActionQueue;

use My::Moose::Role;

use List::BinarySearch qw(binsearch_pos);

use header;

has field 'queued_actions' => (
	isa => Types::ArrayRef,
	default => sub { [] },
);

sub enqueue_action ($self, $action)
{
	my $actions = $self->queued_actions;
	my $index = binsearch_pos { $a->eta <=> $b->eta } $action, $actions->@*;

	splice $actions->@*, $index, 0, $action;
	return;
}

sub _process_actions ($self)
{
	my $actions = $self->queued_actions;
	my $time = server_time;

	while ($actions->@* > 0 && $actions->[0]->finished($time)) {
		my $action = shift $actions->@*;
		next if $action->cancelled;

		my $method = $action->server_method;
		$self->$method($action);
	}
}

after BUILD => sub ($self, @) {
	$self->_add_action(0.05 => '_process_actions');
};

after signal_player_left => sub ($self, $actor) {

	# TODO: dequeue all actor actions
};

