package My::Moose::Role::TracksDirty;

use v5.38;
use My::Moose::Role;

use Scalar::Util qw(refaddr);

# keeps extra data for instances that we want to keep very private
my %dirty;

sub _dirty ($self, $field = undef)
{
	my $selfaddr = refaddr $self;
	if (defined $field) {
		$dirty{$selfaddr}{$field} = 1;
		return;
	}
	else {
		return keys(($dirty{$selfaddr} // {})->%*);
	}
}

sub _clear_dirty ($self)
{
	$dirty{refaddr $self} = {};

	return;
}

sub DEMOLISH ($self, $global_destruction)
{
	delete $dirty{refaddr $self};

	return;
}

