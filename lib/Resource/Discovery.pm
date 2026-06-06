package Resource::Discovery;

use My::Moose;

use Resource::ActorPosition;
use Resource::ActorState;
use Sub::Install;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => HashRef [
		ArrayRef [InstanceOf ['Unit::Actor']]
	],

	default => sub { {} },

	'handles{}' => {
		'_add' => 'set',
	},
);

use constant type => 'discovery';

my @aspects = qw(
	new_actors
	old_actors
);

# NOTE: aspects are full objects
foreach my $aspect (@aspects) {
	Sub::Install::install_sub(
		{
			code => sub ($self, $list) {
				return $self->_add($aspect, $list);
			},
			as => $aspect,
		}
	);
}

sub generate ($self)
{
	my %generated;
	my $subject = $self->subject;

	foreach my $key (@aspects) {
		next unless $subject->{$key};

		$generated{$key} = [
			map {
				$_->id
			} $subject->{$key}->@*
		];
	}

	return \%generated;
}

sub _build_next_resources ($self)
{
	my @resources;
	foreach my $actor (($self->subject->{new_actors} // [])->@*) {
		push @resources, (
			Resource::ActorPosition->new(subject => $actor),
			Resource::ActorState->new(subject => $actor),
		);
	}

	return \@resources;
}

