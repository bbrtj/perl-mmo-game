package Resource::Discovery;

use My::Moose;

use Resource::ActorPosition;
use Resource::ActorEvent;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::HashRef [
		Types::ArrayRef [Types::InstanceOf ['Unit::Actor']]
	],

	default => sub { {} },

	'handles{}' => {
		'_add' => 'set',
	},
);

use constant type => 'discovery';

# NOTE: full objects, to grab their positions
sub new_actors ($self, $list)
{
	return $self->_add('new_actors', $list);
}

sub old_actors ($self, $list)
{
	return $self->_add('old_actors', $list);
}

sub generate ($self)
{
	my @copy = qw(old_actors);

	my %generated;
	my $subject = $self->subject;

	foreach my $key (qw(old_actors new_actors)) {
		if ($subject->{$key}) {
			$generated{$key} = [
				map {
					$_->id
				} $subject->{$key}->@*
			];
		}
	}

	return \%generated;
}

sub _build_next_resources ($self)
{
	my @resources;
	foreach my $actor ($self->subject->{new_actors}->@*) {
		push @resources, Resource::ActorPosition->new(subject => $actor);
		push @resources, Resource::ActorEvent->new(subject => $actor);
	}

	return \@resources;
}

