package My::Moose::Role::TracksDirty;

use v5.32;
use warnings;
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
}

after DESTROY => sub {
	my $self = shift;
	delete $dirty{refaddr $self};
};

1;

