package CLI::export_lore;

use My::Moose -constr;
use Mojo::File qw(path);
use Mojo::JSON qw(encode_json);
use Utils;

use header;

extends 'Mojolicious::Command';

use constant description => 'Exports all lores in the system for the client';
sub usage ($self) { return $self->extract_usage }

sub run ($self, $language = undef)
{
	die 'need a language' unless defined $language;

	Utils->bootstrap_lore;

	my $repo = DI->get('lore_data_repo');
	my %lores = $repo->load_all()->%*;

	my @items;
	foreach my $key (keys %lores) {
		my $item = $lores{$key};
		push @items, {
			LoreId => $item->id,
			LoreName => $item->data->translations->{$language}{name},
			LoreDescription => $item->data->translations->{$language}{desc},
		};
	}

	my $struct = {
		Items => \@items
	};

	my $output = path->child('client')->child('data')->child('lore.json');
	$output->spurt(encode_json $struct);

	say "done, generated in $output";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-lore [LANGUAGE]

