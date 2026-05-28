import importlib.util
import unittest
from importlib.machinery import SourceFileLoader
from pathlib import Path


SCRIPT_PATH = Path(__file__).resolve().parents[3] / "work" / "bin" / "replace-query-params"
SPEC = importlib.util.spec_from_loader(
    "replace_query_params",
    SourceFileLoader("replace_query_params", str(SCRIPT_PATH)),
)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class ReplaceQueryParamsTest(unittest.TestCase):
    def test_substitute_query_params_raises_without_force(self):
        with self.assertRaisesRegex(ValueError, "missing_param"):
            MODULE.substitute_query_params(
                "before {query_param('missing_param')} after",
                {},
            )

    def test_substitute_query_params_leaves_missing_placeholders_with_force(self):
        result = MODULE.substitute_query_params(
            "before {query_param('missing_param')} after",
            {},
            force=True,
        )

        self.assertEqual(result, "before {query_param('missing_param')} after")

    def test_substitute_execution_date_raises_without_force(self):
        with self.assertRaisesRegex(ValueError, "execution_date"):
            MODULE.substitute_execution_date("before {execution_date} after", None)

    def test_substitute_execution_date_leaves_placeholder_with_force(self):
        result = MODULE.substitute_execution_date(
            "before {execution_date} after",
            None,
            force=True,
        )

        self.assertEqual(result, "before {execution_date} after")


if __name__ == "__main__":
    unittest.main()
