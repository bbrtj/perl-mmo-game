package My::Moose::Role::TracksDirty;

use v5.36;
use My::Moose::Role;

use Scalar::Util qw(refaddr);

my %dirty;

sub _dirty
{
	my ($self, $field) = @_;

	my $selfaddr = refaddr $self;
	if (defined $field) {
		$dirty{$selfaddr}{$field} = 1;
		return;
	}
	else {
		return keys(($dirty{$selfaddr} // {})->%*);
	}
}

sub _clear_dirty
{
	my $self = shift;
	$dirty{refaddr $self} = {};

	return;
}

sub DEMOLISH
{
	my $self = shift;
	delete $dirty{refaddr $self};

	return;
}

1;

