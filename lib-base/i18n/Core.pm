package i18n::Core;

use v5.42;

use parent 'Locale::Maketext';
use YAML::PP qw(LoadFile);
use Path::Tiny qw(path);

foreach my $file (glob path(__FILE__)->parent->parent->sibling('i18n')->child('*.yml')) {
	my ($lang) = $file =~ m{/(\w+)\.yml};
	my @lexicon = LoadFile($file);

	my $self = __PACKAGE__;
	my $lexicon_pkg = sprintf '%s::%s', $self, $lang;
	eval "package $lexicon_pkg; use parent -norequire, '$self'; our %Lexicon; 1;"
		or die "error registering lexicon $lexicon_pkg: $@";

	{
		no strict 'refs';
		%{"${lexicon_pkg}::Lexicon"} = map {
			$_->{id}, $_->{str}
		} @lexicon;
	}
}

