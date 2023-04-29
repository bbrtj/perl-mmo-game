package Resource::Discovery;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::HashRef [
		Types::ArrayRef
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

# NOTE: just ids
sub old_actors ($self, $list)
{
	return $self->_add('old_actors', $list);
}

sub generate ($self)
{
	my @copy = qw(old_actors);

	my %generated;
	my $subject = $self->subject;

	foreach my $key (@copy) {
		$generated{$key} = $subject->{$key}
			if $subject->{$key};
	}

	if ($subject->{new_actors}) {
		$generated{new_actors} = [map {
			+{
				id => $_->id,
				x => $_->variables->pos_x,
				y => $_->variables->pos_y,
			}
		} $subject->{new_actors}->@*];
	}

	return \%generated;
}

