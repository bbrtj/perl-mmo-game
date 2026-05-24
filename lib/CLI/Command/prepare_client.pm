package CLI::Command::prepare_client;

use My::Moose;

use CLI::Command::export_config;
use CLI::Command::export_mo;
use CLI::Command::export_lore;
use CLI::Command::export_maps;

use header;

BEGIN { extends 'CLI::Command' }

use constant description => 'Export everything for the client program at once';
use constant usage => __PACKAGE__->extract_usage;

sub run ($self, $language = undef)
{
	unless (defined $language) {
		$self->help;
		return;
	}

	say 'Exporting config';
	CLI::Command::export_config->new->run();

	say 'Exporting lore';
	CLI::Command::export_lore->new->run($language);

	say 'Exporting maps';
	CLI::Command::export_maps->new->run();

	say 'Exporting translations';
	CLI::Command::export_mo->new->run($language);

	say 'All done';
	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION prepare-client [LANGUAGE]

