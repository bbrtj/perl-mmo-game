package CLI::Command::export_lore;

use My::Moose;
use Path::Tiny qw(cwd);
use JSON::MaybeXS qw(encode_json);
use Utils;

use header;

BEGIN { extends 'CLI::Command' }

use constant description => 'Exports all lores in the system for the client';
use constant usage => __PACKAGE__->extract_usage;

sub run ($self, $language = undef)
{
	unless (defined $language) {
		$self->help;
		return;
	}

	my $repo = DI->get('lore_data_repo');
	my %lores = $repo->load_all()->%*;

	my @items;
	foreach my $key (keys %lores) {
		my $item = $lores{$key};
		push @items, {
			LoreId => $item->id,
			LoreName => $item->translations->{$language}{name},
			LoreDescription => $item->translations->{$language}{desc},
		};
	}

	my $struct = {
		Items => \@items
	};

	my $output = cwd->child('client')->child('data')->child('lore.json');
	$output->spew(encode_json $struct);

	say "done, generated in $output";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-lore [LANGUAGE]

