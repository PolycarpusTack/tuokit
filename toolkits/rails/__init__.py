"""
Rails Toolkit for TuoKit
Comprehensive Rails development suite with 25+ tools
"""

from .analyzer import RailsUltimateToolkit
from .generators import (
    ModelGenerator,
    ControllerGenerator,
    ServiceObjectGenerator,
    FormObjectGenerator,
    ApiDocsGenerator
)
from .testing import (
    RSpecGenerator,
    SystemTestGenerator,
    FactoryGenerator
)
from .debugging import (
    ErrorAnalyzer,
    PerformanceAnalyzer,
    QueryOptimizer
)
from .api import (
    GraphQLGenerator,
    RestApiGenerator,
    SerializerGenerator
)

__all__ = [
    'RailsUltimateToolkit',
    'ModelGenerator',
    'ControllerGenerator',
    'ServiceObjectGenerator',
    'FormObjectGenerator',
    'ApiDocsGenerator',
    'RSpecGenerator',
    'SystemTestGenerator',
    'FactoryGenerator',
    'ErrorAnalyzer',
    'PerformanceAnalyzer',
    'QueryOptimizer',
    'GraphQLGenerator',
    'RestApiGenerator',
    'SerializerGenerator'
]