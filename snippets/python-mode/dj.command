# -*- mode: snippet -*-
# name: dj.command
# key: dj.command
# --
import logging

from django.core.management.base import BaseCommand
from django.db import transaction

from retail.models import RetailInventory, FTSInventory


logger = logging.getLogger('${1:fix_multistore}')


class Command(BaseCommand):
    help = '${2:In multistore: set in_stock=0 for items without parent}'

    def add_arguments(self, parser):
        parser.add_argument('-c', action='store_true', dest='commit', help='Commit changes')

    @transaction.atomic
    def handle(self, commit, *args, **options):
        with FTSInventory.turn_off_sync():
            $0pass
