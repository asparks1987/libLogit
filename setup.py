from setuptools import setup, find_packages

setup(
    name='libLogit',
    version='0.0.1a', 
    url='https://github.com/asparks1987/libLogit.git',
    author='Aryn M. Sparks',
    author_email='Aryn.sparks1987@gmail.com',
    description='JSON-configured multi-sink logging helpers',
    packages=find_packages(include=['liblogit', 'liblogit.*']),    
    install_requires=[],
    python_requires='>=3.9',
)