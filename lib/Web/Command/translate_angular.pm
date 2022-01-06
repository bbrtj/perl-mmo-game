package Web::Command::translate_angular;

use My::Moose -constr;
use Mojo::File qw(curfile path);
use Mojo::JSON qw(from_json to_json);
use DI;

use header;

extends 'Mojolicious::Command';

use constant description => 'move translations to angular';
use constant usage => sub ($self) { $self->extract_usage };

sub get_languages ()
{
	return map {
		$_->basename('.po')
	} grep {
		$_->extname eq 'po'
	} curfile->dirname->dirname->dirname->sibling('i18n')->list->each;
}

sub run ($self, @args)
{
	my $template_file = path('i18n/template.json');
	`npx ng extract-i18n --format json --out-file $template_file`;
	my $template = from_json $template_file->slurp;
	my %translations = $template->{translations}->%*;

	for my $lang (get_languages) {
		my %contents = (locale => $lang);

		for my $tkey (keys %translations) {
			# TODO: adjust argument placeholders in translations
			$contents{translations}{$tkey} = _t $translations{$tkey};
		}

		local $i18n::CURRENT_LANG = $lang;
		path('i18n')->child("messages.$lang.json")->spurt(to_json \%contents);
	}

	$template_file->remove;
	return;
}

__END__
=head1 SYNOPSIS
	Usage: APPLICATION translate-angular
