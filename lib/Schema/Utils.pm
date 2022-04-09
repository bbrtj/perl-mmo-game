package Schema::Utils;

use Exporter qw(import);
use Exception::RecordDoesNotExist;
use Exception::SearchCriteriaTooVague;

use header -noclean;

our @EXPORT_OK = qw(
	fetch_single
	ensure_single
);

sub fetch_single ($rs)
{
	my $found = $rs->next;
	Exception::RecordDoesNotExist->throw unless $found;

	return $found;
}

sub ensure_single ($rs)
{
	my $found = fetch_single($rs);
	Exception::SearchCriteriaTooVague->throw if $rs->next;

	return $found;
}

