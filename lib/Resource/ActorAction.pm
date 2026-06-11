package Resource::ActorAction;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => InstanceOf ['Game::Object::Action'],
);

use constant type => 'actor_action';
use constant is_plaintext => true;

sub generate ($self)
{
	my $action = $self->subject;
	my $duration = $action->cancelled ? 0 : $action->duration;

	# actor id
	# action name
	# action duration (0 if cancelled)
	return [
		$action->actor->id,
		$action->lore->id,
		$duration,
	];
}

